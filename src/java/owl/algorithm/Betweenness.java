package owl.algorithm;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Stack;
import java.util.LinkedList;
import java.util.HashMap;
import clojure.lang.IPersistentMap;
import clojure.lang.MapEntry;
import clojure.lang.Sequential;

public class Betweenness {
  private HashMap graph;

  public Betweenness(IPersistentMap graph) {
    this.graph = new HashMap((Map) graph);
  }

  public Number get(HashMap map, Long key, Number base) {
    return map.containsKey(key) ? (Number) map.get(key) : base;
  }

  public HashMap calculate() {
    int size = this.graph.size();
    HashMap betweenness = new HashMap(size);

    for (Iterator keys = this.graph.keySet().iterator(); keys.hasNext();) {
      Long node = (Long) keys.next();

      Stack stack = new Stack();
      HashMap paths = new HashMap(size);
      HashMap shortest = new HashMap(size);
      HashMap dependence = new HashMap(size);
      HashMap distance = new HashMap(size);
      LinkedList queue = new LinkedList();
      shortest.put(node, 1);
      dependence.put(node, 0);
      queue.add(node);

      while(queue.size() > 0) {
        Long visit = (Long) queue.poll();
        stack.push(visit);
        for(Iterator neighbors = ((Collection) this.graph.get(visit)).iterator(); neighbors.hasNext();) {
          Long neighbor = (Long) neighbors.next();
          if (!dependence.containsKey(neighbor)) {
            queue.add(neighbor);
            dependence.put(neighbor, ((int) this.get(dependence, visit, -1)) + 1);
          }

          if ((int) dependence.get(neighbor) == ((int) this.get(dependence, visit, -1)) + 1) {
            shortest.put(neighbor, ((int) this.get(shortest, neighbor, 0)) + ((int) this.get(shortest, visit, 0)));
            if (!paths.containsKey(neighbor)) {
              paths.put(neighbor, new LinkedList());
            }

            ((LinkedList) paths.get(neighbor)).add(visit);
          }
        }
      }

      while(!stack.empty()) {
        Long last = (Long) stack.pop();
        if (paths.containsKey(last)) {
          for (Iterator path = ((LinkedList) paths.get(last)).iterator(); path.hasNext();) {
            Long step = (Long) path.next();
            double ratio = ((int) this.get(shortest, step, 0)) / (double) ((int) shortest.get(last));
            double scale = ((double) this.get(distance, last, 0.0)) + 1;
            distance.put(step, ((double) this.get(distance, step, 0.0)) + ratio * scale);

            if (last != node) {
              betweenness.put(last, ((double) this.get(betweenness, last, 0.0)) + ((double) get(distance, last, 0.0)));
            }
          }
        }
      }
    }

    return betweenness;
  }
}


