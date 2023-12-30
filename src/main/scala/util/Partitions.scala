package freechips.rocketchip.util

import chisel3._
import chisel3.util.{PriorityEncoder, log2Floor}

/** [[Partitions]], for all all i in [1; maxNumPartitions], 
  * distributes i equally-sized bins (partitions) across the possible values of (in)
  * and returns which bin (in) resides inside.
  */
object Partitions {

    def apply(in: UInt, maxNumPartitions: Int): Seq[UInt] = {
        /* Get the maximum value plus one of in */
        val maxIn = 1 << in.getWidth

        /* Create a (k -> v) map where v = in < k.U */
        val cmp_map = collection.mutable.Map(0 -> false.B, maxIn -> true.B)

        /* Iterate over the numbers of partitions which are greater than half of maxNumPartitions */
        val upper_results = (maxNumPartitions/2+1 to maxNumPartitions).map(numPartitions => 
            /* Detect which partition (in) is within using a priority encoder fed with
             * inequality comparisons over the partition boundaries.
             * Use previously computed comparisons.
             */
            PriorityEncoder((1 until numPartitions).map(partition => {
                val ub = ((maxIn*partition)/numPartitions).toInt
                cmp_map.getOrElseUpdate(ub, in < ub.U)
            }).appended(true.B))
        )

        /* The remaining numbers of partitions are half of another number of partitions.
         * This means that the remaining results are bit shifts of those in upper_results.
         */
        val lower_results = (1 to maxNumPartitions/2).map(numPartitions => {
            /* Find maximum k>0 such that (numPartitions * 2^k <= maxNumPartitions) */
            val k = log2Floor(maxNumPartitions/numPartitions)
            /* Now derive this result from the result for (numPartitions*2^k) partitions */
            upper_results((numPartitions << k)-maxNumPartitions/2-1) >> k
        })

        /* Concatenate the results and return */
        lower_results ++ upper_results
    }    
}

/** [[PartitionsOH]], for all all i in [1; maxNumPartitions], 
  * distributes i equally-sized bins (partitions) across the possible values of (in)
  * and returns which bin (in) resides inside in the form of a one-hot value.
  */
object PartitionsOH {

    def apply(in: UInt, maxNumPartitions: Int): Seq[UInt] = {
        /* Get the maximum value plus one of in */
        val maxIn = 1 << in.getWidth

        /* Create a (k -> v) map where v = in < k.U */
        val cmp_map = collection.mutable.Map(0 -> false.B, maxIn -> true.B)

        /* Iterate over the numbers of partitions */
        (1 to maxNumPartitions).map(numPartitions => 
            /* Iterate over the number of partitions */
            (0 until numPartitions).map(partition => {
                /* Detect whether (in) is inside this partition, using previously computed comparisons */
                val lb = ((maxIn*partition)/numPartitions).toInt
                val ub = ((maxIn*(partition+1))/numPartitions).toInt
                !cmp_map.getOrElseUpdate(lb, in < lb.U) && cmp_map.getOrElseUpdate(ub, in < ub.U)
            }).asUInt
        )
    }    
}

