finalizer: ResourceT IO ()
reqSource: ConduitM () ByteString (ResourceT IO) ()

type Source m o = ConduitM () o m ()

