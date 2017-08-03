(defproject aviary/owl "0.0.22"
  :description "Graph analysis in Clojure"
  :url "http://github.com/littlebird/owl"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :javac-options ["-target" "1.8" "-source" "1.8"]
  :java-source-paths ["src/java"]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.taoensso/timbre "4.8.0"]
                 [org.clojure/math.combinatorics "0.1.1"]]
  :plugins [[s3-wagon-private "1.1.2"]]
  :repositories [["private" {:url "s3p://littlebird-maven/releases/"
                             :creds :gpg
                             :sign-releases false}]])
