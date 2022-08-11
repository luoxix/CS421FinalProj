--- Getting Started
--- ===============

--- Testing Your Code
--- -----------------

module Tests where

import Data.List ((\\))
import Data.Array
import Lib

allTests :: [([Bool], String)]
allTests = [
            --- unit tests      
            (tests_build_graph, "ut - build graph")
           , (tests_get_vertices, "ut - get vertices")
           , (tests_get_edges, "ut - get edges")
           , (tests_get_outdegree, "ut - get outdegree")
           , (tests_transposeG, "ut - transpose graph")
           , (tests_get_indegree, "ut - get indegree")
           , (tests_generate_tree, "ut - generate tree at vertex")

            --- Feature Tests
           , (tests_depth_first_forest, "dfs - depth first forest from 1st vertex of graph")
           , (tests_depth_first_search, "dfs - depth first search from specified vertex of graph")
           
            --- DFS Applications Tests
           , (tests_preord_graph, "numbering - get pre-order of depth-first forest from graph")
           , (tests_tabulate_vertices, "numbering - get pre-ordered positions of vertices")
           , (tests_postord_graph, "top sort - get post-order of depth-first forest from graph")
           , (tests_topsort_graph, "top sort - get topological sort from graph")
           , (tests_reachable_vertices, "reachable - get rechable vertices from given vertex in graph")
           ]


--- unit tests
graph = buildG ('a','j') 
                [('a','j'),('a','g'),('b','i'),('b','a'),('c','h'),('c','e'),('e','j'),('e','h'),('e','d'),('f','i'),('g','f'),('g','b')]

tests_build_graph :: [Bool]
tests_build_graph = [ graph ! 'e' == ['d', 'h', 'j'] ]

tests_get_vertices :: [Bool]
tests_get_vertices = [ vertices graph == "abcdefghij" ]

tests_get_edges :: [Bool]
tests_get_edges = [ edges graph == [('a','g'),('a','j'),('b','a'),('b','i'),('c','e'),('c','h'),('e','d'),('e','h'),('e','j'),('f','i'),('g','b'),('g','f')]]

tests_get_outdegree :: [Bool]
tests_get_outdegree = [ show (outdegree graph) 
                    == "array ('a','j') [('a',2),('b',2),('c',2),('d',0),('e',3),('f',1),('g',2),('h',0),('i',0),('j',0)]"
                ]

tests_transposeG :: [Bool]
tests_transposeG = [ graph ! 'c' == ['e', 'h'], (transposeG graph) ! 'e' == ['c'] ]

tests_get_indegree :: [Bool]
tests_get_indegree = [ show (indegree graph) 
                    == "array ('a','j') [('a',1),('b',1),('c',0),('d',1),('e',1),('f',1),('g',1),('h',2),('i',2),('j',2)]"
                ]

tests_generate_tree :: [Bool]
tests_generate_tree = [ generate graph 'e' == Node 'e' [Node 'd' [],Node 'h' [],Node 'j' []] ]

--- Feature Tests
tests_depth_first_forest :: [Bool]
tests_depth_first_forest = [ dff graph == [Node 'a' [Node 'g' [Node 'b' [Node 'i' []],Node 'f' []],Node 'j' []],Node 'c' [Node 'e' [Node 'd' [],Node 'h' []]]] ]

tests_depth_first_search :: [Bool]
tests_depth_first_search = [ dfs graph ['b'] == [Node 'b' [Node 'a' [Node 'g' [Node 'f' [Node 'i' []]],Node 'j' []]]] ]

--- DFS Applications Tests
------ Numbering
tests_preord_graph :: [Bool]
tests_preord_graph = [ preOrd graph == "agbifjcedh" ]

tests_tabulate_vertices :: [Bool]
tests_tabulate_vertices = [ show (tabulate ('a','j') (preOrd graph)) == "array ('a','j') [('a',1),('b',3),('c',7),('d',9),('e',8),('f',5),('g',2),('h',10),('i',4),('j',6)]" ]

------ Topological Sort
tests_postord_graph :: [Bool]
tests_postord_graph = [ postOrd graph == "ibfgjadhec" ]

tests_topsort_graph :: [Bool]
tests_topsort_graph = [ topSort graph == "cehdajgfbi" ]

------ Finding reachable vertices
tests_reachable_vertices :: [Bool]
tests_reachable_vertices = [ reachable graph 'b' == "bagfij" ]
