(ns Matrix-Calculator)

(def mat1 [[1 2 3] [4 5 6] [7 8 9] [10 11 12] [13 14 15]])

(def mat2 [[3 2 1] [6 5 4] [9 8 7]])


(defn sumVector [vec1]
  {:pre [(vector? vec1)] }
  
   (apply + vec1)
 )

(defn sumMatrix [mat1]
  {:pre [(matrix? mat1)] }
  
   (apply + (apply concat mat1))
  )

(defn addVectors [vec1 vec2]
    {:pre [(vector? vec1) (vector? vec2) (= (count vec1) (count vec2)) ] }
   
    (vec 
      (lazy-seq
        (when-let [v1 (seq vec1)]
          (when-let [v2 (seq vec2)]
            (map + v1 v2)
      )
     )
    )
   )
  )

(defn addMatrixes [mat1 mat2]
  {:pre [(sameSize? mat1 mat2)] }
   
   (vec 
     (lazy-seq
       (when-let [m1 (seq mat1)]
         (when-let [m2 (seq mat2)]
           (map addVectors m1 m2)
       )
      )
     )
    )
   )
 

(defn createMultipicationVec [vec1 vec2]
    {:pre [(vector? vec1) (vector? vec2) (= (count vec1) (count vec2)) ]  }
     
     (map * vec1 vec2)
 )

(defn dotProduct [vec1 vec2]
    {:pre [(vector? vec1) (vector? vec2) (= (count vec1) (count vec2)) ]  }
    
     (apply + (map * vec1 vec2))
)


(defn mulBy [s]
  #(* % s)
 )


(defn vecScalarMul [vec1 s]
  {:pre [(vector? vec1)] }
   
   (lazy-seq
     (when-let [v1 (seq vec1)]
       (map (mulBy s) v1)
    )
   )
 )

(defn matScalarMul [mat1 s]
  {:pre [(matrix? mat1)] }
   
   (lazy-seq
      (when-let [m1 (seq mat1)]
          (map #(vecScalarMul % s) m1)
     )
   )
 )


(defn subMatrixes [mat1 mat2]
  {:pre [(sameSize? mat1 mat2)] }
  
   (addMatrixes mat1 (matScalarMul mat2 -1)) 
 )


(defn matTranspose [mat1]
  {:pre [(matrix? mat1)] }
   
   (vec (apply map vector mat1))
 )



(defn matMultipication [mat1 mat2]
  {:pre [(sameColsRows? mat1 mat2)] }
  
   (if (= (count mat1) 0) 
  
      nil
    
     (vec
       (loop [rslt (vector) m1 mat1]
         (if (= (count m1) 0)
            rslt
      
            (recur (conj rslt (vec (map #(dotProduct (first m1) %) (matTranspose mat2)))) (rest m1))
      )      
     )
    )
   )
  )


(defn matMultipication_lazy [mat1 mat2]
  {:pre [(sameColsRows? mat1 mat2)] }
  
   (if (= (count mat1) 0) 
  
     nil    
    
     (lazy-seq
       (when-let [m1 mat1]
         (when-let [m2 mat2]
          (cons (map #(dotProduct (first m1) %) (matTranspose m2)) (matMultipication_lazy (rest m1) m2)) 
      )
     )
    )
   )
  )
 

(defn matPow [mat p]
  {:pre [(sameColsRows? mat mat)] }
    (if (= p 1) 
      mat
    
      (if (even? p)
        (let [rslt (matPow mat (/ p 2))] 
          (matMultipication rslt rslt)
         )
      
        (matMultipication (matPow mat (- p 1)) mat)   
    )  
   )
  )


(defn exec
  ([f m] m)
  ([f m1 m2] (f m1 m2))
  ([f m1 m2 & more] (reduce f (f m1 m2) more))
 )


(defn matrix? [mat]
  (and 
    (vector? mat)
    (apply = true (map vector? mat))
    (apply = (map #(= (count (first mat)) (count %)) mat))
    )
  )

(defn sameSize? [mat1 mat2]
  {:pre [(matrix? mat1) (matrix? mat2)] }
   
   (and
     (= (count mat1) (count mat2))
     (= (count (first mat1)) (count (first mat2)))
    )
  ) 

(defn sameColsRows? [mat1 mat2]
  {:pre [(matrix? mat1) (matrix? mat2)] }
    
    (= (count (first mat1)) (count mat2))
  )