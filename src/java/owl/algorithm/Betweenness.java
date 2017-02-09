package owl.algorithm;

import java.util.*;

public class Betweenness {

    private Map<Long, Collection<Long>> graph;

    public Betweenness(Map<Long, Collection<Long>> graph) {
        this.graph = graph;
    }

    private Long get(Map map, Long key, Integer base) {
        return map.containsKey(key) ? (Long) map.get(key) : base;
    }

    public Map calculate() {
        int size = this.graph.size();
        Map<Long, Number> betweenness = new HashMap<>(size);

        Stack<Long> stack = new Stack<>();
        Map<Long, Collection<Long>> paths = new HashMap<>(size);
        Map<Long, Long> shortest = new HashMap<>(size);
        Map<Long, Long> dependence = new HashMap<>(size);
        Map<Long, Number> distance = new HashMap<>(size);
        Queue<Long> queue = new PriorityQueue<>();

        graph.forEach((node, numbers) -> {
            stack.empty();
            paths.clear();
            shortest.clear();
            dependence.clear();
            distance.clear();
            queue.clear();

            shortest.put(node, 1L);
            dependence.put(node, 0L);
            queue.offer(node);

            queue.forEach(visit -> {
                stack.push(visit);
                Collection<Long> graphValues = graph.get(visit);
                graphValues.forEach((neighbor) -> {

                    Long dependent = get(dependence, visit, -1) + 1;

                    if (!dependence.containsKey(neighbor)) {
                        queue.offer(neighbor);
                        dependence.put(neighbor, dependent);
                    }

                    if (dependence.get(neighbor).equals(dependent)) {
                        Long _short = get(shortest, neighbor, 0) + get(shortest, visit, 0);
                        shortest.put(neighbor,_short);

                        if (!paths.containsKey(neighbor)) {
                            paths.put(neighbor, new LinkedList<>());
                        }
                        paths.get(neighbor).add(visit);
                    }
                });

            });

            stack.forEach(last -> {
                if (paths.containsKey(last)) {
                    paths.get(last).forEach(step -> {
                        Double ratio = get(shortest,step,0).doubleValue() / (shortest.get(last)).doubleValue();
                        Double scale = get(distance,last,0).doubleValue() + 1.0 ;
                        Double dist = get(distance,step,0).doubleValue() + ratio * scale;
                        distance.put(step,dist);

                        if(!last.equals(node)){
                            Double between = get(betweenness,last,0).doubleValue() + get(distance,last,0).doubleValue();

                            betweenness.put(last, between);
                        }
                    });
                }
            });

        });

        return betweenness;

//
//    for (Iterator keys = this.graph.keySet().iterator(); keys.hasNext();) {
//      Long node = (Long) keys.next();
//
//      Stack stack = new Stack();
//      HashMap paths = new HashMap(size);
//      HashMap shortest = new HashMap(size);
//      HashMap dependence = new HashMap(size);
//      HashMap distance = new HashMap(size);
//      LinkedList queue = new LinkedList();
//      shortest.put(node, 1);
//      dependence.put(node, 0);
//      queue.add(node);
//
//      while(queue.size() > 0) {
//        Long visit = queue.poll();
//        stack.push(visit);
//
//        for(Iterator neighbors = ((Collection) this.graph.get(visit)).iterator(); neighbors.hasNext();) {
//          Long neighbor = neighbors.next();
//          if (!dependence.containsKey(neighbor)) {
//            queue.add(neighbor);
//            dependence.put(neighbor, ((int) this.get(dependence, visit, -1)) + 1);
//          }
//
//          if ((int) dependence.get(neighbor) == ((int) this.get(dependence, visit, -1)) + 1) {
//            shortest.put(neighbor, ((int) this.get(shortest, neighbor, 0)) + ((int) this.get(shortest, visit, 0)));
//            if (!paths.containsKey(neighbor)) {
//              paths.put(neighbor, new LinkedList());
//            }
//
//            ((LinkedList) paths.get(neighbor)).add(visit);
//          }
//        }
//      }
//
//      while(!stack.empty()) {
//        Long last = (Long) stack.pop();
//        if (paths.containsKey(last)) {
//          for (Iterator path = ((LinkedList) paths.get(last)).iterator(); path.hasNext();) {
//            Long step = (Long) path.next();
//            double ratio = ((int) this.get(shortest, step, 0)) / (double) ((int) shortest.get(last));
//            double scale = ((double) this.get(distance, last, 0.0)) + 1;
//            distance.put(step, ((double) this.get(distance, step, 0.0)) + ratio * scale);
//
//            if (last != node) {
//              betweenness.put(last, ((double) this.get(betweenness, last, 0.0)) + ((double) get(distance, last, 0.0)));
//            }
//          }
//        }
//      }
//    }

//    return betweenness;
    }
}


