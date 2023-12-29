package freechips.rocketchip.util

import chisel3._
import chisel3.util.{OHToUInt, PriorityEncoder, log2Floor}

object Partitions {

    def apply(in: UInt, maxNumPartitions: Int): Seq[UInt] = {
        /* Get the maximum value plus one of in */
        val maxIn = 1 << in.getWidth

        /* Iterate over the numbers of partitions which are greater than half of maxNumPartitions */
        val upper_results = (maxNumPartitions/2+1 to maxNumPartitions).map(numPartitions => 
            /* Detect which partition (in) is within using a priority encoder fed with
             * inequality comparisons over the partition boundaries.
             */
            PriorityEncoder((1 until numPartitions).map(partition => 
                in < ((maxIn*partition)/numPartitions).toInt.U
            ).appended(true.B))
        )

        /* The remaining numbers of partitions are under half of another number of partitions.
         * This means that the remaining results are bit shifts of those in upper_results .
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

