{
  extras = hackage:
    {
      packages = {
        "ekg" = (((hackage.ekg)."0.4.0.15").revisions).default;
        "libyaml" = (((hackage.libyaml)."0.1.0.0").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "ekg-json" = (((hackage.ekg-json)."0.1.0.6").revisions).default;
        "aeson-options" = (((hackage.aeson-options)."0.1.0").revisions).default;
        "pvss" = (((hackage.pvss)."0.2.0").revisions).default;
        "lrucache" = (((hackage.lrucache)."1.2.0.1").revisions).default;
        "writer-cps-mtl" = (((hackage.writer-cps-mtl)."0.1.1.6").revisions).default;
        "writer-cps-transformers" = (((hackage.writer-cps-transformers)."0.5.6.0").revisions).default;
        "o-clock" = (((hackage.o-clock)."1.0.0.1").revisions).default;
        "ekg-wai" = (((hackage.ekg-wai)."0.1.0.3").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "half" = (((hackage.half)."0.2.2.3").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
        "process" = (((hackage.process)."1.6.5.0").revisions).default;
        "graphviz" = (((hackage.graphviz)."2999.20.0.3").revisions)."cde383c356bc41136ed53cd27e0800f46dbd2185600dd0de18d66d5c49739d94";
        "quickcheck-state-machine" = (((hackage.quickcheck-state-machine)."0.6.0").revisions)."3e4f8df0f6b5d415e3c8840dc75034a63e37f56f5f8cfa1035ded16345235ac4";
        } // {
        typed-transitions = ./typed-transitions.nix;
        typed-protocols = ./typed-protocols.nix;
        ouroboros-network = ./ouroboros-network.nix;
        ouroboros-network-testing = ./ouroboros-network-testing.nix;
        ouroboros-consensus = ./ouroboros-consensus.nix;
        io-sim = ./io-sim.nix;
        io-sim-classes = ./io-sim-classes.nix;
        byron-proxy = ./byron-proxy.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        contra-tracer = ./contra-tracer.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-sl = ./cardano-sl.nix;
        cardano-sl-binary = ./cardano-sl-binary.nix;
        cardano-sl-binary-test = ./cardano-sl-binary-test.nix;
        cardano-sl-util = ./cardano-sl-util.nix;
        cardano-sl-util-test = ./cardano-sl-util-test.nix;
        cardano-sl-infra = ./cardano-sl-infra.nix;
        cardano-sl-infra-test = ./cardano-sl-infra-test.nix;
        cardano-sl-core = ./cardano-sl-core.nix;
        cardano-sl-core-test = ./cardano-sl-core-test.nix;
        cardano-sl-chain = ./cardano-sl-chain.nix;
        cardano-sl-chain-test = ./cardano-sl-chain-test.nix;
        cardano-sl-db = ./cardano-sl-db.nix;
        cardano-sl-db-test = ./cardano-sl-db-test.nix;
        cardano-sl-crypto = ./cardano-sl-crypto.nix;
        cardano-sl-crypto-test = ./cardano-sl-crypto-test.nix;
        cardano-sl-networking = ./cardano-sl-networking.nix;
        cardano-report-server = ./cardano-report-server.nix;
        log-warper = ./log-warper.nix;
        rocksdb-haskell-ng = ./rocksdb-haskell-ng.nix;
        kademlia = ./kademlia.nix;
        network-transport = ./network-transport.nix;
        network-transport-tcp = ./network-transport-tcp.nix;
        network-transport-inmemory = ./network-transport-inmemory.nix;
        universum = ./universum.nix;
        serokell-util = ./serokell-util.nix;
        ether = ./ether.nix;
        transformers-lift = ./transformers-lift.nix;
        cborg = ./cborg.nix;
        cardano-crypto = ./cardano-crypto.nix;
        hedgehog = ./hedgehog.nix;
        canonical-json = ./canonical-json.nix;
        };
      compiler.version = "8.6.4";
      compiler.nix-name = "ghc864";
      };
  resolver = "lts-13.16";
  compiler = "ghc-8.6.4";
  }