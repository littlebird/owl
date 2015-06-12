(defproject aviary/owl "0.0.6"
  :description "Graph analysis in Clojure"
  :url "http://github.com/littlebird/owl"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.taoensso/timbre "3.3.1"]
                 [org.clojure/math.combinatorics "0.1.1"]]
  :plugins [[s3-wagon-private "1.1.2"]]
  :repositories [["private" {:url "s3p://littlebird-maven/releases/"
                             :creds :gpg
                             :sign-releases false}]])
