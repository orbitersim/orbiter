/**
 \page vesselconcept Vessel module concepts

 \section docking_management Docking port management

 Docking ports allow individual vessel objects to connect with each other,
 forming a superstructure. Orbiter automatically calculates the physical
 properties of the superstructure from the properties of the individual
 constituents. In particular, the following properties are managed by
 Orbiter:
 - total mass: The mass of the superstructure is the sum of masses of
   the individual vessels
 - centre of mass. The centre of mass of the superstructure is calculated
   from the individual vessel masses and their relative position
 - inertia tensor: A simplified rigid-body model is applied to calculate
   an inertia tensor for the superstructure.
 - effects of forces: any forces acting on individual vessels (thrust,
   drag, lift, etc.) are transformed into the superstructure frame and
   applied.

 The superstructure model allows to apply the effect of forces calculated
 for individual vessels onto the superstructure. For example, a thrust
 force that acts along the centre of gravity of an individual vessel may
 induce a torque on the superstructure, depending on the relative position
 of the vessel with respect to the superstructure centre of mass.

 Currently, superstructures are only supported in free flight, \e not when
 landed on a planetary surface. The reason is the difficulty of calculating
 the interaction of the composite structure with the surface. This may
 be addressed in the future.

 \section attachment_management Attachment management

 Similar to docking ports, attachment points allow to connect two or more
 vessel objects. There are a few important differences:
 - Docking ports establish peer connections, attachments establish
   parent-child hierarchies: A parent vessel can have multiple attached
   children, but each child can only be attached to a single parent.
 - Attachments use a simplified physics engine: the root parent alone
   defines the object's trajectory (both for freespace and atmospheric
   flight). The children are assumed to have no influence on flight behaviour.
 - Orbiter establishes docking connections automatically if the docking
   ports of two vessels are brought close to each other. Attachment
   connections are only established by API calls.
 - Currently, docking connections only work in freeflight. Attachments
   also work for landed vessels.

 Attachment connections are useful for attaching small objects to larger
 vessels. For example, Orbiter uses attachments to connect payload items
 to the Space Shuttle's cargo bay or the tip of the RMS manipulator arm
 (see Orbitersdk\\samples\\Atlantis).

 Attachment points use an identifier string (up to 8 characters) which can
 provide a method to establish compatibility. For example, the Atlantis
 RMS arm tip will only connect to attachment points with an id string that
 contains "GS" in the first 2 characters (it ignores the last 6 characters).
 Now let's assume somebody creates another Shuttle (say a Buran) with its
 own RMS arm. He could then allow it to
 - grapple exactly the same objects as Atlantis, by checking for "GS".
 - grapple a subset of objects grapplable by Atlantis, by checking
   additional characters, for example "GSX".
 - grapple all objects grapplable by Atlantis, plus additional objects,
   for example by checking for "GS" or "GX".
 - grapple entirely different objects, for example by checking for "GX".

 To connect a satellite into the payload bay, Atlantis uses the id "XS"
 (This means that the payload bay connection can not be used for grappling.
 To allow a satellite to be grappled and stored in the payload bay, it
 must define both a "GS" and an "XS" attachment point).
*/