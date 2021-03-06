(ns reptile.server.core-test
  (:require
    [clojure.spec.alpha :as spec]
    [clojure.test :refer :all]
    [clojure.string :as str]
    [reptile.server.http :refer :all]
    [reptile.server.socket-repl :as repl]))

; use this as a fixture for each test?
(defn evaller [& {:keys [comp-first?]
                  :or   {comp-first? false}}]
  (let [prepl       (repl/shared-prepl {:host :self :port 0 :server-daemon true})
        shared-eval (partial repl/shared-eval prepl)]
    (if comp-first? (comp first shared-eval) shared-eval)))

(deftest ^:basic-prepl-tests basic-prepl-tests
  (testing "Basic Clojure forms"
    (let [shared-eval (evaller :comp-first? true)]

      ; self evaluating forms
      (let [xs ["42" "4.2" ":x" (pr-str "foo")]]
        (doall (map (fn [x]
                      (let [{:keys [val]} (shared-eval x)]
                        (is (= x val))))
                    xs)))

      ; vars
      (let [{:keys [tag val]} (shared-eval "(def x 1)")]
        (is (= :ret tag))
        (is (= "#'user/x" val)))

      (let [{:keys [val]} (shared-eval "x")]
        (is (= "1" val)))

      ; Literals
      (let [{:keys [val]} (shared-eval "[123 \\newline ##Inf nil true :foo]")]
        (is (= "[123 \\newline ##Inf nil true :foo]" val)))

      (let [{:keys [val tag]} (shared-eval "#'x")]
        (is (= :ret tag))
        (is (= "#'user/x" val))))))

(deftest ^:common-invocation-tests common-invocation-tests
  (testing "Standard, side effects and recursion"
    (let [shared-eval (evaller)]

      ; Standard invocations
      (let [resp (shared-eval "(+ 3 4)")
            {:keys [val]} (first resp)]
        (is (= "7" val)))

      (let [resp (shared-eval "(inc x)")
            {:keys [val]} (first resp)]
        (is (= "2" val)))

      (let [resp (shared-eval "(range 2)")
            {:keys [val]} (first resp)]
        (is (= "(0 1)" val)))

      ; Side effects
      (let [resp (shared-eval "(println \"foo\")")
            {:keys [val tag]} (first resp)]
        (is (= 2 (count resp)))
        (is (= "foo\n" val))
        (is (= :out tag))
        (is (= nil (get-in (last resp) [:eval-result :val]))))

      ; Loop / recur
      (let [resp (shared-eval "(loop [results [1]]
                                 (if (= [1 2 3] results)
                                   results
                                   (recur (conj results (inc (last results))))))")
            {:keys [val]} (first resp)]
        (is (= [1 2 3] (read-string val)))))))

(deftest ^:read-lambda-tests read-lambda-tests
  (testing "Lambdas"
    (let [shared-eval (evaller :comp-first? true)]

      (let [{:keys [val]} (shared-eval "(map #(inc %) (range 3))")]
        (is (= '(1 2 3) (read-string val))))

      (let [{:keys [val]} (shared-eval "(map (fn [x] (inc x)) (range 3))")]
        (is (= '(1 2 3) (read-string val)))))))

(deftest ^:reader-char-tests reader-char-tests
  (testing "Reader special characters"
    (let [shared-eval (evaller :comp-first? true)]

      (let [{:keys [val]} (shared-eval "'l33t")]
        (is (= 'l33t (read-string val))))

      (let [{:keys [val]} (shared-eval "(quote l33t)")]
        (is (= "l33t" val)))

      (let [_ (shared-eval "(def atomic (atom 123))")
            {:keys [val]} (shared-eval "@atomic")]
        (is (= "123" val)))

      (let [_ (shared-eval "(defn type-hints [^String s] (clojure.string/trim s))")
            {:keys [val]} (shared-eval "(type-hints \"  Hello-World    \")")]
        (is (= (pr-str "Hello-World") val)))

      (let [_ (shared-eval "(def x 1)")
            _ (shared-eval "(def lst '(a b c))")
            {:keys [val]} (shared-eval "`(fred x ~x lst ~@lst 7 8 :nine)")]
        (is (= "(user/fred user/x 1 user/lst a b c 7 8 :nine)" val)))

      (let [{:keys [val]} (shared-eval "#{1}")]
        (is (= "#{1}" val)))

      (let [{:keys [val]} (shared-eval "(re-find #\"\\s*\\d+\" \"Hello-World42\")")]
        (is (= (pr-str "42") val))))))

(deftest ^:comment-tests comment-tests
  (testing "Various comment styles"
    (let [shared-eval (evaller :comp-first? true)]

      (let [{:keys [tag val form]} (shared-eval "; 42")]
        (is (= tag :err))
        (is (= form "; 42"))
        (let [{:keys [type ex-kind]} (read-string val)]
          (is (= :reader-exception type))
          (is (= :eof ex-kind))))

      (let [{:keys [tag val]} (shared-eval "(comment 42)")]
        (is (= :ret tag))
        (is (nil? (read-string val))))

      (let [{:keys [tag val form]} (shared-eval "#_ xx")]
        (is (= :err tag))
        (is (= form "#_ xx"))
        (let [{:keys [type ex-kind]} (read-string val)]
          (is (= :reader-exception type))
          (is (= :eof ex-kind)))))))

(deftest ^:multi-form-tests multi-form-tests
  (testing "Multiple forms in a buffer"
    (let [shared-eval (evaller)]
      (let [resp          (shared-eval "(def x 1) x")
            first-result  (first resp)
            second-result (last resp)]

        (is (= 2 (count resp)))

        (is (= :ret (:tag first-result)))
        (is (= "#'user/x" (:val first-result)))

        (is (= :ret (:tag second-result)))
        (is (= "1" (:val second-result)))))))

(deftest ^:add-lib-tests add-lib-tests
  (testing "Test spec / add-lib"
    (let [shared-eval (evaller)]

      (let [add-ok (shared-eval "(use 'clojure.tools.deps.alpha.repl)
                                 (add-lib 'org.clojure/test.check {:mvn/version \"0.9.0\"})")]
        (is (nil? (read-string (:val (first add-ok)))))
        (is (boolean? (read-string (:val (last add-ok))))))

      (let [spec-ok (shared-eval "(require '[clojure.spec.alpha :as s])
                                  (s/valid? even? 10)")]
        (is (nil? (read-string (:val (first spec-ok)))))
        (is (true? (read-string (:val (last spec-ok))))))

      (let [gen-ok (shared-eval "(require '[clojure.spec.gen.alpha :as gen])
                                 (gen/generate (s/gen int?))")]
        (is (nil? (read-string (:val (first gen-ok)))))
        (is (int? (read-string (:val (last gen-ok)))))))))

(deftest ^:graceful-fail-tests graceful-fail-tests
  (testing "Test graceful failures for syntax and spec errors"
    (let [shared-eval (evaller :comp-first? true)]

      (let [{:keys [err-source ex-data tag val]} (shared-eval "(prn \"000")]
        (is (and (false? ex-data)
                 (= :err tag)
                 (= :read-forms err-source)))
        (is (= "EOF while reading string" val)))

      (let [{:keys [err-source ex-data tag val]} (shared-eval "(")]
        (is (and (false? ex-data)
                 (= :err tag)
                 (= :read-forms err-source)))
        (is (= "EOF while reading" val)))

      (let [{:keys [tag val]} (shared-eval "(defn x (+ 1 2))")
            {:keys [cause via trace data phase]}
            (binding [*default-data-reader-fn* repl/default-reptile-tag-reader]
              (read-string val))
            problems (::spec/problems data)
            spec     (::spec/spec data)
            value    (::spec/value data)
            args     (::spec/args data)]

        (is (= :ret tag))
        (is (= :macro-syntax-check phase))
        (is (= cause "Call to clojure.core/defn did not conform to spec."))
        (is (= 2 (count (filter :message via))))
        (is (and (vector? trace) (> (count trace) 10)))
        (is (= 2 (count problems)))
        (is (= 2 (count (keys spec))))
        (is (= '(x (+ 1 2)) value))
        (is (= '(x (+ 1 2)) args))))))

(deftest ^:in-namespaces in-namespaces
  (testing "Testing the support and use of namespaces"
    (let [shared-eval (evaller :comp-first? true)]

      ; Functions
      (let [{:keys [val tag]} (shared-eval "(defn ns-x2 [x] (+ x x))")]
        (is (= :ret tag))
        (is (= val "#'user/ns-x2")))

      (let [{:keys [val tag]} (shared-eval "*ns*")]
        (is (= :ret tag))
        (is (str/ends-with? val "\"user\"]"))
        (is (str/starts-with? val "#object")))

      (let [{:keys [val]} (shared-eval "(ns-x2 17)")]
        (is (= "34" val)))

      ; TODO investigate ns support
      #_(let [{:keys [val tag] :as x} (shared-eval "(in-ns 'repl-test)")]
          (println "111 val" val "x" x)
          (is (= :ret tag))
          (is (str/ends-with? val "\"repl-test\"]"))
          (is (str/starts-with? val "#object"))

          (let [{:keys [val tag] :as x} (shared-eval "*ns*")]
            (is (= :ret tag))
            (is (str/ends-with? val "\"repl-test\"]"))
            (is (str/starts-with? val "#object")))

          (let [{:keys [val tag] :as x} (shared-eval "(in-ns 'user)")]
            (is (= :ret tag))
            (is (str/ends-with? val "\"user\"]"))
            (is (str/starts-with? val "#object")))))))


