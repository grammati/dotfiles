{:user
 {:plugins      [[lein-ancient "0.6.10"]
                 [lein-cloverage "1.0.9"]
                 [lein-pprint "1.1.2"]
                 [lein-ns-dep-graph "0.2.0-SNAPSHOT"]]
  :repl-options
  {:init (do (load-file (.getAbsolutePath (clojure.java.io/file (System/getenv "HOME") ".lein/user.clj")))
             (user/inject-repl-utils))}}
 
 :repl
 {
  ;; :plugins      [[cider/cider-nrepl "0.15.0-SNAPSHOT"]
  ;;                [refactor-nrepl "2.3.0-SNAPSHOT"]]
  :dependencies [[org.clojure/tools.namespace "0.3.0-alpha4"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [im.chit/vinyasa "0.3.4"]
                 [spyscope "0.1.6"]]}}
