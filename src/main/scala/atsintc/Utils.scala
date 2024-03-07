package freechips.rocketchip.atsintc

import chisel3._
import chisel3.util._

class DataArray(capacity: Int, dataWidth: Int) extends Module {
    val io = IO(new Bundle {
        val enq = Flipped(Decoupled(UInt(dataWidth.W)))
        val deq = Decoupled(UInt(dataWidth.W))
        val position = Input(UInt(log2Up(capacity).W))
        val length = Output(UInt((log2Up(capacity) + 1).W))
    })
    val size = capacity
    private val mem = RegInit(VecInit(Seq.fill(capacity)(0.U(dataWidth.W))))

    val length = RegInit(0.U((log2Up(capacity) + 1).W))
    io.length := length

    private val deq_valid = RegNext(io.deq.ready && (length > 0.U))
    private val enq_ready = RegNext(io.enq.valid && (length < capacity.U))

    // dequeue: when the consumer is ready to receive the data && the queue has data
    when(io.deq.ready && (length > 0.U) && deq_valid) {
        length := length - 1.U
        for(i <- 0 until capacity - 1) {
            mem(i) := mem(i + 1)
        }
    }

    // enqueue: when the producer has prepared the data && the queue is not full
    when(io.enq.valid && (length < capacity.U) && enq_ready) {
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
    io.enq.ready := enq_ready
}

class PriorityQueue(numPrio: Int, capacity: Int, dataWidth: Int) extends Module {
    val io = IO(new Bundle {
        val enqs = Vec(numPrio, Flipped(DecoupledIO(UInt(dataWidth.W))))
        val deq = Decoupled(UInt(dataWidth.W))
    })
    private val index_blocks = RegInit( VecInit(Seq.fill(numPrio + 1) { 0.U(log2Up(capacity).W) }))
    private val sels = Cat(Seq.tabulate(numPrio) { i => io.enqs(numPrio - i - 1).valid })
    private val current_index = Cat(0.U(1.W), PriorityEncoder(sels))
    private val position = RegNext(index_blocks(current_index + 1.U))

    private val data_array = Module(new DataArray(capacity, dataWidth))

    data_array.io.position := position
    data_array.io.enq.bits := io.enqs(current_index).bits
    data_array.io.enq.valid := sels > 0.U
    io.deq <> data_array.io.deq

    private val enq_readys = Seq.tabulate(numPrio) { i => RegNext(sels(i)) }
    for(i <- 0 until numPrio) {
        io.enqs(i).ready := enq_readys(i)
    }

    // enqueue, the index behind the target priority + 1
    for(i <- 0 until numPrio + 1) {
        when(data_array.io.enq.valid && data_array.io.enq.ready && (i.U > current_index)) {
            index_blocks(i) := index_blocks(i) + 1.U
        }
    }

    // dequeue, the position in the index_blocks which is not zero, - 1 
    for(i <- 0 until numPrio + 1) {
        when(io.deq.ready && io.deq.valid && index_blocks(i) > 0.U) {
            index_blocks(i) := index_blocks(i) - 1.U
        }
    }
}


class PQWithExtIntrHandler(numIntr: Int, bq_capacity: Int, numPrio: Int, capacity: Int, dataWidth: Int) extends Module {
    val io = IO(new Bundle {
        val enqs = Vec(numPrio, Flipped(DecoupledIO(UInt(dataWidth.W))))
        val deq = Decoupled(UInt(dataWidth.W))
        val intrs = Vec(numIntr, Input(Bool()))
        val intrh_enqs = Vec(numIntr, Flipped(DecoupledIO(UInt(dataWidth.W))))
    })

    // Connect the priority queue
    private val pq = Module(new PriorityQueue(numPrio, capacity, dataWidth))
    io.deq <> pq.io.deq
    // except priority 0
    for(i <- 1 until numPrio) {
        pq.io.enqs(i) <> io.enqs(i)
    }

    // connect interrupt handler queue
    private val inFlights0 = Seq.tabulate(numIntr) { i => 
        RegNext(io.intrs(i))
    }
    private val inFlights1 = Seq.tabulate(numIntr) { i => 
        RegNext(io.intrs(i) && !inFlights0(i))
    }
    private val intr_hqs = Seq.fill(numIntr) { 
        val q = Module(new DataArray(bq_capacity, dataWidth))
        q.io.position := q.io.length
        q
    }
    private val arb = Module(new Arbiter(UInt(dataWidth.W), numIntr + 1))
    for(i <- 0 until numIntr) {
        intr_hqs(i).io.enq <> io.intrh_enqs(i)
        intr_hqs(i).io.deq.ready := (io.intrs(i) && !inFlights0(i)) || inFlights1(i)
        // arb.io.in(i) <> intr_hqs(i).io.deq
        arb.io.in(i).bits := RegNext(intr_hqs(i).io.deq.bits)
        arb.io.in(i).valid := intr_hqs(i).io.deq.valid

    }
    arb.io.in(numIntr) <> io.enqs(0)
    pq.io.enqs(0) <> arb.io.out

}

