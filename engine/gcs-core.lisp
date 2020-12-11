(in-package :konig)

;;; fundamental idea:
;;; we have different component 'types': each is type has it's own vector
;;; these vectors are all stored in a bigger vector.

;;; the 'components' are structs: just plain data
;;; we use generational indexes to access the vector & manage deletions

;;; often, components are arranged into a 'graph' with indexes being used as 'pointers' between elements
;;; for example, we may iterate over the 'renderables' array in our render system, , 


