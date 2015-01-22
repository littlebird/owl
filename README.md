# owl

Scry into the nature of the network.

![OWL](https://github.com/plexusengine/owl/blob/master/resources/public/img/owl.jpg)

## Usage

```clj
(ns tundra
  (:require [owl.betweenness :as owl]))

(owl/network-betweenness
 {1 '(2 5) 2 '(3 5) 3 '(4) 4 '() 5 '(4)})

---> {2 1, 5 3/2, 3 1/2, 4 0}
```

## License

Copyright © 2015 Little Bird

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
