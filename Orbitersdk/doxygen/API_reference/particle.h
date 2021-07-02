/**
  \page particle Particle Streams HowTo

  Particle streams are a component of Orbiter graphics clients (see
  \ref graphics).

  Particle streams can be used to create visual effects for contrails,
  exhaust and plasma streams, reentry heating, condensation, etc.

  The management of particle streams is almost entirely the responsibility of
  the graphics client. The orbiter core notifies the client only
  - to request a new particle stream for a vessel object
  - to detach a stream from its object (e.g. if the object is deleted)

  The implementation details for the particle streams, including render options,
  are left to the client.


  \section particle1 Adding particle stream support

  To add particle stream support to a graphics client, the following steps are
  required:
  - Create one or more classes derived from oapi::ParticleStream
  - Overload the particle stream-related callback methods of
    oapi::GraphicsClient, including
	- oapi::GraphicsClient::clbkCreateParticleStream()
	- oapi::GraphicsClient::clbkCreateExhaustStream()
	- oapi::GraphicsClient::clbkCreateReentryStream()

  By default, these methods return NULL pointers, i.e. don't provide particle
  stream support. Your overloaded methods should create an instance of an
  appropriate derived particle stream class and return a pointer to it.

  \b Important: The client must keep track of all particle streams created.
  In particular, the orbiter core never deletes any particle streams it has
  obtained from the client. Particle stream management and cleanup must be
  provided by the client.

  \section particle2 Attaching and detaching streams

  Once a particle stream has been created, it must be connected to a vessel
  instance (provided by the hVessel parameter in each of the particle
  stream-related callback functions of the graphics client). To connect the
  particle stream to the vessel, use one of the oapi::ParticleStream::Attach()
  methods using the provided vessel handle.
  The particle emission point and emission direction are relative to the
  associated vessel.

  Sometimes Orbiter will call the oapi::ParticleStream::Detach() method for a
  stream. This is usually in response to deletion of the vessel. Therefore,
  the stream should no longer make use of the vessel reference after it has
  been detached. In particular, no new particles should be generated.

  \b Important: After Orbiter has detached a particle stream, it will no longer
  access it. The client is free to delete the particle stream instance once
  it has been detached. Generally, the stream should be deleted after all the
  remaining particles in the stream have expired.

  \section particle3 Deleting streams

  Generally, streams should only be deleted after they have been detached and
  after all remaining particles have expired. Deleting a stream with active
  particles will create a visual inconsistency and should be avoided. The only
  exception is the cleanup at the end of a simulation session.

  When a stream is deleted while still attached to its object, Orbiter will
  call the stream's Detach method during the destruction process.

 */