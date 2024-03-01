package freechips.rocketchip.atsintc

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.ShiftQueue


class PriorityQueue(prioritys: Int, entriesPerQueue: Int, dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val deq = Decoupled(UInt(dataWidth.W))
    val enq = Flipped(Decoupled(UInt(dataWidth.W)))
    val enq_prio = Flipped(Valid(Input(UInt(log2Up(prioritys).W))))
  })

  private val inner_queues = Seq.fill(prioritys) { Module(new ShiftQueue(UInt(dataWidth.W), entriesPerQueue)) }
  for(i <- 0 until prioritys) {
    inner_queues(i).io.deq.ready := false.B
    inner_queues(i).io.enq.valid := false.B
  }
  for(i <- 0 until prioritys) {
    when(io.deq.ready && inner_queues(i).io.count > 0.U) {
      io.deq.valid := true.B
      io.deq.bits := inner_queues(i).io.deq.deq()
    }
  }
  for(i <- 0 until prioritys) {
    when(io.enq_prio.valid && io.enq_prio.bits === i.U) {
      io.enq.ready := true.B
      inner_queues(i).io.enq.enq(io.enq.bits)
    }
  }
}

class DataArray(capacity: Int, dataWidth: Int) extends Module {
    val io = IO(new Bundle {
        val enq = Flipped(Decoupled(UInt(dataWidth.W)))
        val deq = Decoupled(UInt(dataWidth.W))
        val position = Input(UInt(log2Up(capacity).W))
    })
    val size = capacity
    private val mem = RegInit(VecInit(Seq.fill(capacity)(0.U(dataWidth.W))))

    private val length = RegInit(0.U((log2Up(capacity) + 1).W))

    private val deq_valid = RegNext(io.deq.ready && (length > 0.U))
    private val enq_valid = RegNext(io.enq.valid && (length < capacity.U))

    // dequeue: when the consumer is ready to receive the data && the queue has data
    when(io.deq.ready && (length > 0.U) && deq_valid) {
        length := length - 1.U
        for(i <- 0 until capacity - 1) {
            mem(i) := mem(i + 1)
        }
    }

    // enqueue: when the producer has prepared the data && the queue is not full
    when(io.enq.valid && (length < capacity.U) && enq_valid) {
      length := length + 1.U
      for(i <- 1 until capacity - 1) {
        when(i.U >= io.position + 1.U) {
          mem(i) := mem(i - 1)
        }
      }
      mem(io.position) := io.enq.bits
    }
    
    // output
    io.deq.bits := Mux(deq_valid, mem(0), 0.U)
    io.deq.valid := deq_valid
    io.enq.ready := enq_valid
}
