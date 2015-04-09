# owl

Scry into the nature of the network.

![OWL](https://github.com/littlebird/owl/blob/master/resources/public/img/owl.jpg)

## Usage

```clj
(ns tundra
  (:require [owl.betweenness :as owl]))

(owl/network-betweenness
 {:a '(:b :e) :b '(:c :e) :c '(:d) :d '() :e '(:d)})

---> {:b 1, :e 3/2, :c 1/2, :d 0}
```

## License

Copyright © 2015 Little Bird

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
