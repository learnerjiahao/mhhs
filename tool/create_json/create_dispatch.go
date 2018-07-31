package main

import (
	"github.com/jessevdk/go-flags"
	"os"
	"math"
	//"github.com/json-iterator/go"
	//"fmt"
	"strconv"
	"log"
	"bytes"
	"encoding/json"
	"io/ioutil"
	"time"
	"fmt"
)


func getUpstreamId(nodeId uint64) (leftUpstream uint64, rightUpstream uint64) {
	return nodeId*2+1, nodeId*2+2
}

func getDownstreamId(nodeId uint64) (downstreamId uint64) {
	return uint64(math.Floor(float64((nodeId-1))/2))
}

func getProcessorIdBynodeId(nodeId uint64, prorank uint64) (locate_proid uint64) {
	return nodeId % prorank
}

func solveNodeIdByIndex(index uint64, proid uint64, prorank uint64) (nodeId uint64) {
	return index * prorank + proid
}

func getDownstreams(upNodeId uint64, processor_rank uint64, downstreams *[] interface{}) {

	if (upNodeId == 0) {
		return
	}

	downstream := make(map[string] interface{})
	downstream["node_id"] = getDownstreamId(upNodeId)
	downstream["locate"] = getProcessorIdBynodeId(getDownstreamId(upNodeId), processor_rank)

	*downstreams = append(*downstreams, downstream)
}



func getUpstreams(downNodeId uint64, processor_rank uint64, node_rank uint64, upstreams *[] interface{}) {

	if (downNodeId >= (node_rank-1) / 2){
		return
	}

	upstreamLeft :=make(map[string]interface{})
	upstreamRight :=make(map[string]interface{})

	upstreamLeftNodeId, upstreamRightNodeId := getUpstreamId(downNodeId)

	upstreamLeft["node_id"] = upstreamLeftNodeId
	upstreamLeft["locate"] = getProcessorIdBynodeId(upstreamLeftNodeId, processor_rank)
	*upstreams = append(*upstreams, upstreamLeft);

	upstreamRight["node_id"] = upstreamRightNodeId
	upstreamRight["locate"] = getProcessorIdBynodeId(upstreamRightNodeId, processor_rank)
	*upstreams = append(*upstreams, upstreamRight);

}

func getNodeCountInOneProcessor(proid uint64, allNodeRank uint64, prorank uint64) (nodeCount uint64) {
	if(proid < (allNodeRank % prorank)) {
		return uint64(math.Ceil(float64(allNodeRank)/float64(prorank)))
	}else {
		return uint64(math.Floor(float64(allNodeRank)/float64(prorank)))
	}
}

func getNodesOnOneProcessor (proid uint64, allNodeRank uint64, prorank uint64, pnodes *[]interface{}) {
	//node := make(map[string] interface{})
	var index uint64 = 0;
	nodeCount := getNodeCountInOneProcessor(proid, allNodeRank, prorank)
	for ; index < nodeCount; index++ {
		node := make(map[string] interface{})

		nodeId := solveNodeIdByIndex(index, proid, prorank);
		node["nodeId"] = nodeId;

		downstreams := make([]interface{},0, 1)
		getDownstreams(nodeId, prorank, &downstreams)
		node["downstreams"] = downstreams

		upstreams := make([]interface{},0, 2)
		getUpstreams(nodeId, prorank, allNodeRank, &upstreams)
		node["upstreams"] = upstreams

		*pnodes = append(*pnodes, node)
	}
}

func goParaer(depth uint64, prorank uint64, FilePathPre string)  {

	var allNodesRank uint64 = uint64(math.Pow(2.0, float64(depth))) - 1
	//fmt.Print(allNodesRank, "\n")
	//var json = jsoniter.ConfigCompatibleWithStandardLibrary

	// todo parallel
	var proid uint64 = 0
	for ; proid < prorank ; proid++  {

		datas := make(map[string] interface{})
		datas["prank"] = prorank
		datas["proid"] = proid;

		var NodesCountOneProcessor uint64 = getNodeCountInOneProcessor(proid, allNodesRank, prorank)
		datas["nrank"] = NodesCountOneProcessor

		nodes := make([]interface{}, 0, NodesCountOneProcessor)

		getNodesOnOneProcessor(proid, allNodesRank, prorank, &nodes)

		datas["nodes"] = nodes;

		jdatas, err := json.Marshal(datas)

		if err != nil {
			log.Fatalln(err)
		}

		var out bytes.Buffer
		err = json.Indent(&out, jdatas, "", "\t")

		if err != nil {
			log.Fatalln(err)
		}

		dispatchFilePath := FilePathPre + "_" + strconv.Itoa(int(proid))
		err = ioutil.WriteFile(dispatchFilePath, out.Bytes(), 0666)

		if err != nil {
			log.Fatalln(err)
		}
	}
}

func main()  {
	var opts struct {
		Prorank uint64 `short:"p" long:"prorank" description:"the rank of used processors"`
		Depth uint64 `short:"d" long:"Depth" description:"the depth of completed binary tree"`
		FilePathPre string `short:"o" long:"output" description:"the prefix of dispatch files"`
	}

	_, err := flags.ParseArgs(&opts, os.Args[1:])

	if err != nil {
		log.Fatalln(err)
	}

	startTime := time.Now().Unix()
	//go goParaer(opts.Depth, opts.Prorank, opts.FilePathPre)
	goParaer(opts.Depth, opts.Prorank, opts.FilePathPre)
	ruTime := time.Now().Unix() - startTime

	fmt.Print("go high runTime : ", ruTime, "s\n")
}
