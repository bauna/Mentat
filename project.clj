(defproject Mentat "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java" "src/test/java"]
  :test-paths ["test" "src/test/clojure"]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"local" "file:repo"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.flatland/ordered "1.5.2"]
                 [com.microsoft/z3 "4.3.2.06a4a3599"]
                 [dorothy "0.0.4"]]
  :dev-dependencied [[no-man-is-an-island/lein-eclipse "2.0.0"]]
  :jvm-opts [~(str "-Djava.library.path=native/" 
                   (. System getProperty "path.separator") 
                   (. System getProperty "java.library.path"))])
