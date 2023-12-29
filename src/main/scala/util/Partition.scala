package freechips.rocketchip.util

import chisel3._
import chisel3.util.OHToUInt
import chisel3.util.log2Ceil
import chisel3.util.log2Floor
import chisel3.experimental.prefix
import chisel3.util.PriorityEncoder

object Partitions {

    def apply(in: UInt, maxNumPartitions: Int): Vec[UInt] = {
        /* Get the maximum value plus one of in */
        val max_in = 1 << in.getWidth

        /* Iterate over the numbers of partitions which are greater than half of maxNumPartitions */
        val upper_results = (maxNumPartitions/2+1 to maxNumPartitions).map(num_partitions => 
            /* Detect which partition (in) is within using a priority encoder fed with
             * inequality comparisons over the partition boundaries.
             */
            PriorityEncoder((1 until num_partitions).map(partition => 
                in < ((max_in*partition)/num_partitions).toInt.U
            ).appended(true.B))
        )

        /* The remaining numbers of partitions are under half of another number of partitions.
         * This means that the remaining results are bit shifts of those in upper_results .
         */
        val lower_results = (1 to maxNumPartitions/2).map(num_partitions => {
            /* Find maximum k>0 such that (num_partitions * 2^k <= maxNumPartitions) */
            val k = log2Floor(maxNumPartitions/num_partitions)
            /* Now derive this result from the result for (num_partitions*2^k) partitions */
            upper_results((num_partitions << k)-maxNumPartitions/2-1) >> k
        })

        /* Concatenate the results and return */
        VecInit(lower_results ++ upper_results)
    }    
}

