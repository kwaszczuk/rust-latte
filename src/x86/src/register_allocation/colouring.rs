use crate::register_allocation::interference_graph::{InterferenceGraph};
use std::collections::{HashMap, HashSet};
use std::hash::{Hash};
use std::cmp::{max};
use std::fmt::{Debug};

#[derive(Debug, PartialEq, Clone)]
pub struct Colouring<Node, Color>
    where Node: PartialEq + Hash + Eq + Clone + Debug,
          Color: PartialEq + Hash + Eq + Clone + Debug
{
    pub total_colors_used: usize,
    pub results: HashMap<Node, Option<Color>>,
}

fn max_cardinality_search<Node: Hash + Eq + Clone + Debug>(mut graph: InterferenceGraph<Node>) -> Vec<Node> {
    let find_max_weight = |w : &HashMap<Node, i32>| -> Option<(Node, i32)> {
        let mut max_weight = -1;
        let mut ret = None;
        for (node, weight) in w {
            if max_weight < *weight {
                max_weight = weight.clone();
                ret = Some((node.clone(), weight.clone()));
            }
        }

        ret
    };

    let mut weights: HashMap<Node, i32> = graph.nodes.iter()
        .map(|n| (n.clone(), 0))
        .collect();
    let mut ordering = vec![];

    let n = graph.nodes.len();
    for _ in 0..n {
        let (node, _) = find_max_weight(&weights).unwrap();
        ordering.push(node.clone());

        for next in graph.edges.get(&node).unwrap() {
            match weights.get_mut(&next) {
                Some(val) => *val += 1,
                None => {},
            }
        }

        graph.nodes.remove(&node);
        graph.edges.remove(&node);
        weights.remove(&node);
    }

    ordering
}

impl<Node, Color> Colouring<Node, Color>
    where Node: PartialEq + Clone + Eq + Hash + Debug,
          Color: PartialEq + Clone + Eq + Hash + Debug
{
    pub fn get(
        graph: &InterferenceGraph<Node>,
        color_set: &Vec<Color>,
        pre_colouring: &HashMap<Node, Color>
    ) -> Self {
        let mut colouring: HashMap<Node, Option<Color>> = pre_colouring.iter()
            .map(|(reg, color)| (reg.clone(), Some(color.clone())))
            .collect();

        let ordering = max_cardinality_search(graph.clone());

        // greedy colouring
        for node in &ordering {
            // check if node wasn't precoloured
            if !colouring.contains_key(&node) {
                let mut used_colors: HashSet<Color> = HashSet::new();
                for neighbour in graph.edges.get(node).unwrap() {
                    match colouring.get(neighbour) {
                        Some(Some(color)) => {
                            used_colors.insert(color.clone());
                        },
                        _ => {},
                    };
                }

                let mut unused_color = None;
                for color_idx in 0..color_set.len() {
                    let color = color_set[color_idx].clone();
                    if !used_colors.contains(&color) {
                        unused_color = Some(color.clone());
                        break;
                    }
                }

                colouring.insert(node.clone(), unused_color);
            }
        }

        let mut used_colors = HashSet::new();
        for (_, color) in &colouring {
            if let Some(color) = color {
                used_colors.insert(color.clone());
            }
        }

        Colouring {
            total_colors_used: used_colors.len(),
            results: colouring,
        }
    }
}
