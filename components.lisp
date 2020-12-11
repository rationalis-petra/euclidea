
(defstruct actuator
  type          ; variable giving the type of the actuator, e.g. push/pull or rotate
  instruction   ; telling the actuator what instruction to perform 
  machine)      ; an id which tells the actuator which machine it is a part of 
  
(defstruct agent
  agent_type    ; essentially a faction tag
  intelligence) ; a thread contianing the ai

(defstruct attachment
  id            ; what it is attached to
  displacment)  ; what displacement it is supposed to maintain
  ;; TODO: make more generic attachment, perhaps? ideally contains rotation, ball&socket, ...
  
(defstruct camera
  theta         ; angle of inclination from camera to target
  phi           ; the azmithal angle from the x-axis to the target
  r)            ; the sperical radial distance from player to camera

(defstruct info
  name          ; name to be used as identifier, a "pretty id"
  tags)         ; a list of tags

(defstruct model
  mesh          ; reference to mesh
  texture       ; reference to texture
  ;; normal map
  ;; material map
  position      ; pos, rot, scale may be different from their rigidbody counterparts
  rotation
  scale)

(defstruct rigidbody
  mass          ; mass in kgs
  position      ; position in x, y, z space (1 = 1m)
  velocity      ; d/dt (position)
  force         ; d/dt (velocity)
  mesh)         ; collision mesh
