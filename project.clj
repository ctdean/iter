(defproject com.ctdean/iter
  "0.1.1"
  :description "A smart looping iterator"
  :dependencies [
                 [org.clojure/clojure "1.6.0"]
                 [medley "0.5.5"]
                 [org.clojure/core.match "0.3.0-alpha4"]

                 ]
  :repositories {"internal" {:url "s3://standard-releases/releases/"
                             :username :env/aws_access_key_id
                             :passphrase :env/aws_secret_access_key
                             :sign-releases false}}
  :plugins [[lein-maven-s3-wagon "0.2.4"]]
  :min-lein-version "2.0.0")
