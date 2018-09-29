(ns reptile.server.core-test
  (:require [clojure.test :refer :all]
            [reptile.server.http :refer :all]
            [reptile.server.socket-repl :as repl]
            [clojure.string :as string]))

(defn evaller [& {:keys [comp-first?]
                  :or   {comp-first? false}}]
  (let [prepl       (repl/shared-prepl {:host :self :port 0 :server-daemon true})
        shared-eval (partial repl/shared-eval prepl)]
    (if comp-first? (comp first shared-eval) shared-eval)))

(deftest basic-prepl-tests
  (testing "Basic Clojure forms"
    (let [shared-eval (evaller :comp-first? true)]

      ; Self evaluating forms
      (let [{:keys [val]} (shared-eval "42")]
        (is (= 42 val)))

      (let [{:keys [val]} (shared-eval "4.2")]
        (is (= 4.2 val)))

      (let [{:keys [val]} (shared-eval ":x")]
        (is (= :x val)))

      (let [{:keys [val]} (shared-eval "\"foo\"")]
        (is (= "foo" val)))

      (let [{:keys [tag val]} (shared-eval "(def x 1)")]
        (is (= :ret tag))
        (is (= 'var (first val))))

      (let [{:keys [val]} (shared-eval "x")]
        (is (= 1 val)))

      ; Literals
      (let [{:keys [val]} (shared-eval "[123 \\newline ##Inf nil true :foo]")]
        (is (= [123 \newline ##Inf nil true :foo] val)))

      (let [{:keys [val tag]} (shared-eval "#'x")]
        (is (= :ret tag))
        (is (= 'var (first val))))

      ; Functions
      (let [{:keys [val tag]} (shared-eval "(defn x2 [x] (+ x x))")]
        (is (= :ret tag))
        (is (= 'var (first val))))

      (let [{:keys [val]} (shared-eval "(x2 4)")]
        (is (= 8 val))))))

(deftest common-invocation-tests
  (testing "Standard, side effects and recursion"
    (let [shared-eval (evaller)]

      ; Standard invocations
      (let [resp (shared-eval "(+ 3 4)")
            {:keys [val]} (first resp)]
        (is (= 7 val)))

      (let [resp (shared-eval "(inc x)")
            {:keys [val]} (first resp)]
        (is (= 2 val)))

      (let [resp (shared-eval "(range 2)")
            {:keys [val]} (first resp)]
        (is (= (range 2) val)))

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
        (is (= [1 2 3] val))))))

(deftest read-lambda-tests
  (testing "Lambdas"
    (let [shared-eval (evaller :comp-first? true)]

      (let [{:keys [val]} (shared-eval "(map #(inc %) (range 3))")]
        (is (= '(1 2 3) val)))

      (let [{:keys [val]} (shared-eval "(map (fn [x] (inc x)) (range 3))")]
        (is (= '(1 2 3) val))))))

(deftest reader-char-tests
  (testing "Reader special characters"
    (let [shared-eval (evaller :comp-first? true)]

      (let [{:keys [val]} (shared-eval "'l33t")]
        (is (= 'l33t val)))

      (let [{:keys [val]} (shared-eval "(quote l33t)")]
        (is (= 'l33t val)))

      (let [_ (shared-eval "(def atomic (atom 123))")
            {:keys [val]} (shared-eval "@atomic")]
        (is (= 123 val)))

      (let [_ (shared-eval "(defn type-hints [^String s] (clojure.string/trim s))")
            {:keys [val]} (shared-eval "(type-hints \"  Hello-World    \")")]
        (is (= "Hello-World" val)))

      (let [_ (shared-eval "(def x 1)")
            _ (shared-eval "(def lst '(a b c))")
            {:keys [val]} (shared-eval "`(fred x ~x lst ~@lst 7 8 :nine)")]
        (is (= '(user/fred user/x 1 user/lst a b c 7 8 :nine) val)))

      (let [{:keys [val]} (shared-eval "#{1}")]
        (is (= #{1} val)))

      (let [{:keys [val]} (shared-eval "(re-find #\"\\s*\\d+\" \"Hello-World42\")")]
        (is (= "42" val))))))

(deftest comment-tests
  (testing "Various comment styles"
    (let [shared-eval (evaller :comp-first? true)]

      (let [{:keys [tag val]} (shared-eval "; 42")]
        (is (= :err tag))
        (is (empty? val)))

      (let [{:keys [tag val]} (shared-eval "(comment 42)")]
        (is (= :ret tag))
        (is (nil? val)))

      (let [{:keys [tag val]} (shared-eval "#_ xx")]
        (is (= :err tag))
        (is (empty? val))))))

(deftest multi-form-tests
  (testing "Multiple forms in a buffer"
    (let [shared-eval (evaller)]
      (let [resp          (shared-eval "(def x 1) x")
            first-result  (first resp)
            second-result (last resp)]

        (is (= 2 (count resp)))

        (is (= :ret (:tag first-result)))
        (is (= 'var (first (:val first-result))))

        (is (= :ret (:tag second-result)))
        (is (= 1 (:val second-result)))))))

(deftest add-lib-tests
  (testing "Test spec / add-lib"
    (let [shared-eval (evaller)]

      (let [add-ok (shared-eval "(use 'clojure.tools.deps.alpha.repl)
                                 (add-lib 'org.clojure/test.check {:mvn/version \"0.9.0\"})")]
        (is (= nil (:val (first add-ok))))
        (is (boolean? (:val (last add-ok)))))

      (let [spec-ok (shared-eval "(require '[clojure.spec.alpha :as s])
                                  (s/valid? even? 10)")]
        (is (= nil (:val (first spec-ok))))
        (is (true? (:val (last spec-ok)))))

      (let [gen-ok (shared-eval "(require '[clojure.spec.gen.alpha :as gen])
                                 (gen/generate (s/gen int?))")]
        (is (= nil (:val (first gen-ok))))
        (is (int? (:val (last gen-ok))))))))

(deftest graceful-fail-tests
  (testing "Test graceful failures for syntax and spec errors"
    (let [shared-eval (evaller :comp-first? true)]

      (let [{:keys [err-source tag val]} (shared-eval "(")]
        (is (and (= :err tag) (= :shared-eval err-source)))
        (is (empty? val)))

      (let [{:keys [err-source tag val]} (shared-eval "(defn x (+ 1 2))")]
        (is (and (= :err tag) (= :process-form err-source)))
        (is (string/starts-with? val "#error"))))))
