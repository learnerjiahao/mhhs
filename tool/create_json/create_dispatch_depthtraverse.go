package main

import (
	"os"
	"log"
	"time"
	"fmt"
	"github.com/jessevdk/go-flags"
	"io/ioutil"
	"encoding/json"
	"strconv"
	"bytes"
)

type JsonStruct struct {
}

func NewJsonStruct() *JsonStruct {
	return &JsonStruct{}
}

func (jst *JsonStruct) Load(filename string, v interface{}) {
	//ReadFile函数会读取文件的全部内容，并将结果以[]byte类型返回
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Errorf("read ", filename, " error!!!\n")
		return
	}

	//读取的数据为json格式，需要进行解码
	err = json.Unmarshal(data, v)
	if err != nil {
		fmt.Errorf("unmarshal json file ", filename, " error!!!\n")
		return
	}
}

type Node struct {
	NodeId      int
	Weight      int
	Upstreams   []int
	Downstreams []int
	Locate      int
	IsTravled   bool
}

type Nodes struct {
	Count uint64
	Nodes []Node
}

func createDispatch(proceRank int, inputFilePath string, outputFilePathPre string) {
	// 1. get all nodes
	jsonParse := NewJsonStruct()
	nodesFromJSON := Nodes{}
	jsonParse.Load(inputFilePath, &nodesFromJSON);
	mapNodes := make(map[int]Node)

	// ava weight
	weightSum := 0
	for _, value := range nodesFromJSON.Nodes{
		value.Locate = -1
		value.IsTravled = false
		mapNodes[value.NodeId] = value
		weightSum = weightSum + value.Weight
	}
	weightAvaSumOnePro := float64(weightSum) / float64(proceRank)

	// 2. find all leaf nodes(source nodes) which has no upstreams
	nodeQueue := NewQueue()
	for _, value := range mapNodes {
		if len(value.Upstreams) <= 0 {
			//fakeSrcNode.Downstreams = append(fakeSrcNode.Downstreams, value.NodeId)
			nodeQueue.Offer(value.NodeId)
		}
	}

	// 3. deeply traverse
	weightSumOnePro := 0
	locate := 0
	dispatchNodes := make([][]int, proceRank)

	for !nodeQueue.IsEmpty() {

		nowTravleNodeId,_ := nodeQueue.Poll().(int)
		nodeNowTravel := mapNodes[nowTravleNodeId]

		if nodeNowTravel.IsTravled {
			continue
		}

		if weightAvaSumOnePro - float64(weightSumOnePro)  >= (float64(nodeNowTravel.Weight) / 2)  { // todo
			weightSumOnePro = weightSumOnePro + nodeNowTravel.Weight
		} else {
			locate ++
			weightSumOnePro = nodeNowTravel.Weight
		}

		nodeNowTravel.Locate = locate

		for _, downNodeId := range nodeNowTravel.Downstreams {
			nodeQueue.Offer(downNodeId)
		}

		nodeNowTravel.IsTravled = true

		mapNodes[nodeNowTravel.NodeId] = nodeNowTravel

		dispatchNodes[locate] = append(dispatchNodes[locate], nowTravleNodeId)
	}

	// 4. write dispatch json file
	for pro := 0; pro < proceRank; pro++ {
		datas := make(map[string] interface{})
		datas["prank"] = proceRank
		datas["proid"] = pro;
		datas["nrank"] = len(dispatchNodes[pro])
		nodes := make([]interface{}, 0, len(dispatchNodes[pro]))
		for _, value := range dispatchNodes[pro] {

			node := make(map[string] interface{})

			node["nodeId"] = value;

			downstreams := make([]interface{}, len(mapNodes[value].Downstreams))
			for index, downNodeId := range mapNodes[value].Downstreams {
				downstream := make(map[string]interface{})
				downstream["node_id"] = downNodeId
				downstream["locate"] = mapNodes[downNodeId].Locate
				downstreams[index] = downstream
			}
			node["downstreams"] = downstreams

			upstreams := make([]interface{}, len(mapNodes[value].Upstreams))
			for index, upNodeId := range mapNodes[value].Upstreams {
				upstream := make(map[string]interface{})
				upstream["node_id"] = upNodeId
				upstream["locate"] = mapNodes[upNodeId].Locate
				upstreams[index] = upstream
			}
			node["upstreams"] = upstreams

			nodes = append(nodes, node);
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

			dispatchFilePath := outputFilePathPre + ".json_" + strconv.Itoa(int(pro))
			err = ioutil.WriteFile(dispatchFilePath, out.Bytes(), 0666)

			if err != nil {
				log.Fatalln(err)
			}
		}

	}
}

func main() {
	var opts struct {
		Prorank           int `short:"p" long:"prorank" description:"the rank of used processors"`
		InputFilePath     string `short:"i" long:"input" description:"the input json file of nodes's topology"`
		OutputFilePathPre string `short:"o" long:"output" description:"the prefix of dispatch files"`
	}

	_, err := flags.ParseArgs(&opts, os.Args[1:])

	if err != nil {
		log.Fatalln(err)
	}

	startTime := time.Now().Unix()
	createDispatch(opts.Prorank, opts.InputFilePath, opts.OutputFilePathPre)
	ruTime := time.Now().Unix() - startTime

	fmt.Println("go high runTime : ", ruTime, "s")
}




type Element interface{}

type Queue interface {
	Offer(e Element)    //向队列中添加元素
	Poll()   Element    //移除队列中最前面的元素
	Clear()  bool       //清空队列
	Size()  int            //获取队列的元素个数
	IsEmpty() bool   //判断队列是否是空
}

type  sliceEntry struct{
	element []Element
}

func NewQueue() *sliceEntry {
	return &sliceEntry{}
}

//向队列中添加元素
func (entry *sliceEntry) Offer(e Element) {
	entry.element = append(entry.element,e)
}

//移除队列中最前面的额元素
func (entry *sliceEntry) Poll() Element {
	if entry.IsEmpty() {
		fmt.Println("queue is empty!")
		return nil
	}

	firstElement := entry.element[0]
	entry.element = entry.element[1:]
	return firstElement
}

func (entry *sliceEntry) Clear() bool {
	if entry.IsEmpty() {
		return false
	}
	for i:=0 ; i< entry.Size() ; i++ {
		entry.element[i] = nil
	}
	entry.element = nil
	return true
}

func (entry *sliceEntry) Size() int {
	return len(entry.element)
}

func (entry *sliceEntry) IsEmpty() bool {
	if len(entry.element) == 0 {
		return true
	}
	return false
}