CrossClj – cross-referencing the Clojure(Script) universe
======================

Note: CrossClj website will be closed on 2018-12-31. After that date some links in this page will become invalid.

CrossClj is a tool to explore the interconnected Clojure universe. 

[http://crossclj.info](http://crossclj.info)

As an example, you can find all the uses of reduce across all projects, or find all the functions called map. 

[http://crossclj.info/fun/clojure.core/reduce.html](http://crossclj.info/fun/clojure.core/reduce.html)

[http://crossclj.info/search?q=map](http://crossclj.info/search?q=map)

Or you can list all the projects using ring. 

[http://crossclj.info/ns/ring/ring-core/latest/project.clj.html](http://crossclj.info/ns/ring/ring-core/latest/project.clj.html)

You can also walk the source code across different projects.

[http://crossclj.info/ns/org.clojure/tools.analyzer/latest/clojure.tools.analyzer.html#_macroexpand](http://crossclj.info/ns/org.clojure/tools.analyzer/latest/clojure.tools.analyzer.html#_macroexpand)

You can also explore Clojurescript (and cljx) projects, 

[http://crossclj.info/ns/org.clojure/clojurescript/latest/cljs.core.html](http://crossclj.info/ns/org.clojure/clojurescript/latest/cljs.core.html)

[http://crossclj.info/ns/garden/latest/garden.stylesheet.cljs.html](http://crossclj.info/ns/garden/latest/garden.stylesheet.cljs.html)


and browse auto-generated documentation of both standard namespaces or other projects. 

[http://crossclj.info/doc/org.clojure/clojure/latest/clojure.string.html](http://crossclj.info/doc/org.clojure/clojure/latest/clojure.string.html)

You can also do a full-text search on the whole generated documentation archive for all projects.

[http://crossclj.info/docs.html](http://crossclj.info/docs.html)

There is a sub-site focused on ClojureScript

[http://crossclj.info/cljs](http://crossclj.info/cljs)


### Source code – caveats and acknowledgements

CrossClj is the first non-trivial application I developed in Clojure, in 2013, 
while I was still learning and experimenting with the language. Also, it started as a "quick hack" 
experiment, that was never meant for publication as source code.

As such, both as a Clojure application and a web application, CrossClj is not 
"aesthetically pleasant", or safe, by my current standards, and it makes use of a number of anti-patterns.

Still, I decided to make the source code available, in the hope that it will be useful to some people that have expressed interest in it.

As of now (June 2018) I'm not interested in maintaining this project anymore.

CrossClj makes use of other open-source artifacts. Some of these have been cloned in a new namespace, and included directly in CrossClj's source, 
in a way similar to what Andy Fingerhut's Dolly tool does:

https://github.com/jafingerhut/dolly

This is necessary to minimize external references while using these tools to analyze (read+eval) arbitrary third-party code.
Open source projects that are cloned and used within CrossClj include:

- tools.reader https://github.com/clojure/tools.reader
- tools.analyzer https://github.com/clojure/tools.analyzer
- Cheshire https://github.com/dakrone/cheshire
- Codox https://github.com/weavejester/codox (this has been heavily modified, but it's still the backbone of CrossClj's generator of HTML documentation)
- Pomegranate https://github.com/cemerick/pomegranate

### Implementation overview

CrossClj, is actually a somewhat simple system:

1) it takes a list of published clojure projects from clojars 
https://github.com/clojars/clojars-web/wiki/Data

2) it iterates on them, and for each one

3) it downloads the full dependencies' transitive closure using the Maven toolchain, and temporarily add them to the runtime

4) it parses all the source code with tools.analyzer 

5) ...which produces a very detailed AST (see http://clojure.github.io/tools.analyzer/spec/quickref.html for reference) which includes, for each symbol in the source code, both the position in the source code itself, and the fully qualified reference (ie. var, local binding...)

6) this information is then put in a global "coordinate system" for the cross-referenced entities,  <group, project, version, namespace, var> and the project-local qualified names are resolved into the full coordinates (e.g. project.clj is used to resolve the libraries)

7) source code is decorated, and data is indexed using Apache Lucene, which is very good also for fulltext search

Some further discussion on the implementation of CrossClj may be found here:

[https://www.reddit.com/r/Clojure/comments/8ls9j4/a_new_clojure_documentation_effort/dzvy6ql/](https://www.reddit.com/r/Clojure/comments/8ls9j4/a_new_clojure_documentation_effort/dzvy6ql/)

License
=======

Copyright © 2013-2018 Francesco Bellomi

Distributed under the Eclipse Public License, the same as Clojure.
