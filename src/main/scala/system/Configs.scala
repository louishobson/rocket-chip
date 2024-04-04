// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.system

import org.chipsalliance.cde.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink.TLLatencies

class WithJtagDTMSystem extends freechips.rocketchip.subsystem.WithJtagDTM
class WithDebugSBASystem extends freechips.rocketchip.subsystem.WithDebugSBA
class WithDebugAPB extends freechips.rocketchip.subsystem.WithDebugAPB

class BaseConfig extends Config(
  new WithDefaultMemPort ++
  new WithDefaultMMIOPort ++
  new WithDefaultSlavePort ++
  new WithTimebase(BigInt(1000000)) ++ // 1 MHz
  new WithDTS("freechips,rocketchip-unknown", Nil) ++
  new WithNExtTopInterrupts(2) ++
  new BaseSubsystemConfig
)

class DefaultConfig extends Config(new WithNBigCores(1) ++ new WithCoherentBusTopology ++ new BaseConfig)

class EntanglingIPrefetcherBaseConfig(
  nSets: Int = 128,
  nWays: Int = 4
) extends Config(
  new WithL1ICacheProfiling ++ 
  new WithNPerfCounters(16) ++
  
  new WithoutTLMonitors ++
    
  new WithL2Latency(TLLatencies.block(15)) ++
  new WithControlBusLatency(TLLatencies.block(15)) ++
  new WithMemoryBusLatency(TLLatencies.queue(100, 10)) ++

  new WithInclusiveCache(outerLatencyCycles = 100, nWays = 4, capacityKB = 128) ++

  new WithL1ICacheWays(nWays) ++ 
  new WithL1ICacheSets(nSets) ++ 
  
  new DefaultConfig
)

class EntanglingIPrefetcherConfig(
  nSets : Int = 128,
  nWays: Int = 4,
  entanglingNSets: Int = 128,
  entanglingNWays: Int = 4,
  entanglingAddrBits: Int = 44, 
  maxEntanglings: Int = 4
) extends Config(
  new WithEntanglingIPrefetcherIssueLatency(24) ++
  new WithEntanglingIPrefetcherTableSize(entanglingNSets, entanglingNWays) ++
  new WithEntanglingIPrefetcherCompressionCfg(entanglingAddrBits, maxEntanglings) ++
  new WithEntanglingIPrefetcherNPrefetchMSHRs(3) ++ 
  new WithEntanglingIPrefetcher ++ 
  new EntanglingIPrefetcherBaseConfig(nSets, nWays)
)

class EntanglingIPrefetcherNoPrefetcherBaselineConfig extends EntanglingIPrefetcherBaseConfig

class EntanglingIPrefetcherMaxEntanglings1CompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=26, maxEntanglings=1) //26
class EntanglingIPrefetcherMaxEntanglings2CompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=32, maxEntanglings=2) //32, 16
class EntanglingIPrefetcherMaxEntanglings3CompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=38, maxEntanglings=3) //38, 19, 12
class EntanglingIPrefetcherMaxEntanglings4CompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=44, maxEntanglings=4) //44, 22, 14, 11
class EntanglingIPrefetcherMaxEntanglings5CompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=52, maxEntanglings=5) //52, 26, 17, 13, 10
class EntanglingIPrefetcherMaxEntanglings6CompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=60, maxEntanglings=6) //60, 30, 20, 15, 12, 10

class EntanglingIPrefetcherMaxEntanglings1UncompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=26*1, maxEntanglings=1)
class EntanglingIPrefetcherMaxEntanglings2UncompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=26*2, maxEntanglings=2) 
class EntanglingIPrefetcherMaxEntanglings3UncompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=26*3, maxEntanglings=3) 
class EntanglingIPrefetcherMaxEntanglings4UncompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=26*4, maxEntanglings=4) 
class EntanglingIPrefetcherMaxEntanglings5UncompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=26*5, maxEntanglings=5) 
class EntanglingIPrefetcherMaxEntanglings6UncompressedConfig extends EntanglingIPrefetcherConfig(entanglingAddrBits=26*6, maxEntanglings=6) 

class EntanglingIPrefetcherICacheSize064Sets2WaysNoPrefetcherConfig extends EntanglingIPrefetcherBaseConfig(nSets=64, nWays=2)
class EntanglingIPrefetcherICacheSize064Sets4WaysNoPrefetcherConfig extends EntanglingIPrefetcherBaseConfig(nSets=64, nWays=4)
class EntanglingIPrefetcherICacheSize128Sets2WaysNoPrefetcherConfig extends EntanglingIPrefetcherBaseConfig(nSets=128, nWays=2)
class EntanglingIPrefetcherICacheSize128Sets4WaysNoPrefetcherConfig extends EntanglingIPrefetcherBaseConfig(nSets=128, nWays=4)
class EntanglingIPrefetcherICacheSize256Sets2WaysNoPrefetcherConfig extends EntanglingIPrefetcherBaseConfig(nSets=256, nWays=2)
class EntanglingIPrefetcherICacheSize256Sets4WaysNoPrefetcherConfig extends EntanglingIPrefetcherBaseConfig(nSets=256, nWays=4)
class EntanglingIPrefetcherICacheSize512Sets2WaysNoPrefetcherConfig extends EntanglingIPrefetcherBaseConfig(nSets=512, nWays=2)
class EntanglingIPrefetcherICacheSize512Sets4WaysNoPrefetcherConfig extends EntanglingIPrefetcherBaseConfig(nSets=512, nWays=4)

class EntanglingIPrefetcherICacheSize064Sets2WaysWithPrefetcherConfig extends EntanglingIPrefetcherConfig(nSets=64, nWays=2)
class EntanglingIPrefetcherICacheSize064Sets4WaysWithPrefetcherConfig extends EntanglingIPrefetcherConfig(nSets=64, nWays=4)
class EntanglingIPrefetcherICacheSize128Sets2WaysWithPrefetcherConfig extends EntanglingIPrefetcherConfig(nSets=128, nWays=2)
class EntanglingIPrefetcherICacheSize128Sets4WaysWithPrefetcherConfig extends EntanglingIPrefetcherConfig(nSets=128, nWays=4)
class EntanglingIPrefetcherICacheSize256Sets2WaysWithPrefetcherConfig extends EntanglingIPrefetcherConfig(nSets=256, nWays=2)
class EntanglingIPrefetcherICacheSize256Sets4WaysWithPrefetcherConfig extends EntanglingIPrefetcherConfig(nSets=256, nWays=4)
class EntanglingIPrefetcherICacheSize512Sets2WaysWithPrefetcherConfig extends EntanglingIPrefetcherConfig(nSets=512, nWays=2)
class EntanglingIPrefetcherICacheSize512Sets4WaysWithPrefetcherConfig extends EntanglingIPrefetcherConfig(nSets=512, nWays=4)

class EntanglingIPrefetcherTableSize016Sets2WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=16, entanglingNWays=2)
class EntanglingIPrefetcherTableSize016Sets4WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=16, entanglingNWays=4)
class EntanglingIPrefetcherTableSize016Sets8WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=16, entanglingNWays=8)
class EntanglingIPrefetcherTableSize032Sets2WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=32, entanglingNWays=2)
class EntanglingIPrefetcherTableSize032Sets4WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=32, entanglingNWays=4)
class EntanglingIPrefetcherTableSize032Sets8WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=32, entanglingNWays=8)
class EntanglingIPrefetcherTableSize064Sets2WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=64, entanglingNWays=2)
class EntanglingIPrefetcherTableSize064Sets4WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=64, entanglingNWays=4)
class EntanglingIPrefetcherTableSize064Sets8WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=64, entanglingNWays=8)
class EntanglingIPrefetcherTableSize128Sets2WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=128, entanglingNWays=2)
class EntanglingIPrefetcherTableSize128Sets4WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=128, entanglingNWays=4)
class EntanglingIPrefetcherTableSize128Sets8WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=128, entanglingNWays=8)
class EntanglingIPrefetcherTableSize256Sets2WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=256, entanglingNWays=2)
class EntanglingIPrefetcherTableSize256Sets4WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=256, entanglingNWays=4)
class EntanglingIPrefetcherTableSize256Sets8WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=256, entanglingNWays=8)
class EntanglingIPrefetcherTableSize512Sets2WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=512, entanglingNWays=2)
class EntanglingIPrefetcherTableSize512Sets4WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=512, entanglingNWays=4)
class EntanglingIPrefetcherTableSize512Sets8WaysConfig extends EntanglingIPrefetcherConfig(entanglingNSets=512, entanglingNWays=8)

class SimpleConfig extends Config(new With1SimpleCore ++ new WithCoherentBusTopology ++ new BaseConfig)

class DefaultBufferlessConfig extends Config(new WithBufferlessBroadcastHub ++ new DefaultConfig)
class DefaultSmallConfig extends Config(new WithNSmallCores(1) ++ new WithCoherentBusTopology ++ new BaseConfig)
class DefaultRV32Config extends Config(new WithRV32 ++ new DefaultConfig)
class DefaultFP16Config extends Config(new WithFP16 ++ new DefaultConfig)

class BitManipCryptoConfig extends Config(new WithBitManip ++ new WithCryptoNIST ++ new WithCryptoSM ++ new DefaultConfig)
class BitManipCrypto32Config extends Config(new WithBitManip ++ new WithCryptoNIST ++ new WithCryptoSM ++ new DefaultRV32Config)

class HypervisorConfig extends Config(new WithHypervisor ++ new DefaultConfig)

class DualBankConfig extends Config(new WithNBanks(2) ++ new DefaultConfig)
class DualCoreConfig extends Config(new WithNBigCores(2) ++ new WithCoherentBusTopology ++ new BaseConfig)
class DualChannelConfig extends Config(new WithNMemoryChannels(2) ++ new DefaultConfig)
class EightChannelConfig extends Config(new WithNMemoryChannels(8) ++ new DefaultConfig)

class DualChannelDualBankConfig extends Config(
  new WithNMemoryChannels(2) ++
  new WithNBanks(4) ++ new DefaultConfig
)

class RoccExampleConfig extends Config(new WithRoccExample ++ new DefaultConfig)

class HeterogeneousTileExampleConfig extends Config(
  new WithNBigCores(n = 1) ++
  new WithNMedCores(n = 1) ++
  new WithNSmallCores(n = 1) ++
  new WithCoherentBusTopology ++
  new BaseConfig
)

class Edge128BitConfig extends Config(
  new WithEdgeDataBits(128) ++ new DefaultConfig
)
class Edge32BitConfig extends Config(
  new WithEdgeDataBits(32) ++ new DefaultConfig
)

class SingleChannelBenchmarkConfig extends Config(new DefaultConfig)
class DualChannelBenchmarkConfig extends Config(new WithNMemoryChannels(2) ++ new SingleChannelBenchmarkConfig)
class QuadChannelBenchmarkConfig extends Config(new WithNMemoryChannels(4) ++ new SingleChannelBenchmarkConfig)
class OctoChannelBenchmarkConfig extends Config(new WithNMemoryChannels(8) ++ new SingleChannelBenchmarkConfig)

class TinyConfig extends Config(
  new WithNoMemPort ++
  new WithNMemoryChannels(0) ++
  new WithNBanks(0) ++
  new With1TinyCore ++
  new WithIncoherentBusTopology ++
  new BaseConfig
)

class MemPortOnlyConfig extends Config(
  new WithNoMMIOPort ++
  new WithNoSlavePort ++
  new DefaultConfig
)

class MMIOPortOnlyConfig extends Config(
  new WithNoSlavePort ++
  new WithNoMemPort ++
  new WithNMemoryChannels(0) ++
  new WithNBanks(0) ++
  new WithIncoherentTiles ++
  new WithScratchpadsOnly ++
  new WithIncoherentBusTopology ++
  new DefaultConfig
)

class BaseFPGAConfig extends Config(new BaseConfig ++ new WithCoherentBusTopology)
class DefaultFPGAConfig extends Config(new WithNSmallCores(1) ++ new BaseFPGAConfig)

class CloneTileConfig extends Config(new WithCloneRocketTiles(7) ++ new WithNBigCores(1) ++ new WithCoherentBusTopology ++ new BaseConfig)
