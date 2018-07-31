#/usr/bin/python3
#encoding=utf-8
import sys
from optparse import OptionParser
import json


def checkUpstreamOrDwonstreamNodes(streamsArray, data, rank_index, node_index, tag):
    for i in range(len(streamsArray)):
        nodeId = streamsArray[i]["node_id"];
        locate = streamsArray[i]["locate"];
        # 1.在rank_id为locate的进程上寻找node_id为nodeId的节点
        # 1.1 寻找rank_id为locate的进程在data中的index
        target_locate_index = -1
        for j in range(len(data["dispatch"])):
            if locate == data["dispatch"][j]["rank_id"]:
                target_locate_index = j
        if target_locate_index == -1:
            sys.stderr.write("\t\t It's impossible that one node locates at P:{0}".format(locate) + "\n")
            return False

        # 1.2 寻找node_id为nodeId的节点
        target_nodeId_index = -1
        for j in range(len(data["dispatch"][target_locate_index]["nodes"])):
            if nodeId == data["dispatch"][target_locate_index]["nodes"][j]["node_id"]:
                target_nodeId_index = j;
        if target_nodeId_index == -1:
            sys.stderr.write(
                "\t\t node whose id is {0}".format(nodeId) + " can't locate at P:{0}".format(locate) + "\n")
            return False

        # 到这一步，代表上游或者下游的节点在指定进程上存在
        # 进一步进行反向校验:保证你是我的上游并且我是你的下游或者你是我的下游并且我是你的上游
        upOrDownStreamStrRevert = ''
        streamType = ''
        if tag:  # 若校验某个节点的上游,反向校验需检查其所有上游节点的下游节点是否包含该节点
            streamType = 'upstream'
            upOrDownStreamStrRevert = 'downstream'
        else:  # 若校验某个节点的下游,反向校验需检查其所有下游节点的上游节点是否包含该节点
            streamType = 'downstream'
            upOrDownStreamStrRevert = 'upstream'

        checkResvert = False
        for j in range(
                len(data["dispatch"][target_locate_index]["nodes"][target_nodeId_index][upOrDownStreamStrRevert])):
            if data["dispatch"][target_locate_index]["nodes"][target_nodeId_index][upOrDownStreamStrRevert][j][
                "locate"] == data["dispatch"][rank_index]["rank_id"] and \
                    data["dispatch"][target_locate_index]["nodes"][target_nodeId_index][upOrDownStreamStrRevert][j][
                        "node_id"] == data["dispatch"][rank_index]["nodes"][node_index]["node_id"]:
                checkResvert = True
                break

        if not checkResvert:
            sys.stderr.write("\t\t on P{0}: 在节点(目标节点)编号为{1}".format(data["dispatch"][rank_index]["rank_id"]
                                                                    , data["dispatch"][rank_index]["nodes"][node_index][
                                                                        "node_id"])
                             + " 的{0}节点{1}的{2}的".format(streamType, data["dispatch"][target_locate_index]["nodes"][
                target_nodeId_index]["node_id"]
                                                        , upOrDownStreamStrRevert)
                             + "中找不到目标节点或者其记录的目标节点所在进程号错误\n")
            return False

    return True


def checkDispatchJson(jfile):
    with open(jfile, 'r') as f:
        data = json.load(f)

    # 1.进程数一致性的确保
    print('processor check: {0},{1}'.format(len(data["dispatch"]), data["header"]["ranks"]))
    if len(data["dispatch"]) != data["header"]["ranks"]:
        sys.stderr.write("Error!!! the dispatch json file ["
                         + jfile + "]: \n\t the count of processor({0}".format(len(data["dispatch"]))
                         + ") in 'dispatch' is not equal to 'ranks'({0}".format(data["header"]["ranks"]) + ")")
        sys.exit()

    for i in range(len(data["dispatch"])):
        # 2.每个进程上分配的节点个数一致性的确保
        print('node check: {0},{1}'.format(len(data["dispatch"][i]["nodes"]), data["dispatch"][i]["nodes_count"]))
        if len(data["dispatch"][i]["nodes"]) != data["dispatch"][i]["nodes_count"]:
            sys.stderr.write("Error!!! the dispatch json file ["
                             + jfile + "]: \n\t the count of nodes({0}".format(
                len(data["dispatch"][i]["nodes"])) + ") in 'nodes' "
                             + " is not equal to 'nodes_count'({0}".format(data["dispatch"][i]["nodes_count"])
                             + ") on processor {0}".format(data["dispatch"][i]["rank_id"]) + "\n")

        # 3.确保每个进程上各个节点上下游的一致性
        for j in range(len(data["dispatch"][i]["nodes"])):
            # 校验该节点的上游的对应关系
            if not checkUpstreamOrDwonstreamNodes(data["dispatch"][i]["nodes"][j]["upstream"],
                                                  data, i, j, True):
                sys.stderr.write("Error!!! the dispatch json file ["
                                 + jfile + "]: \n\t "
                                 + "on processor {0}".format(data["dispatch"][i]["rank_id"])
                                 + ": at least one of the upstream nodeId of {0}".format(
                    data["dispatch"][i]["nodes"][j]["node_id"])
                                 + " can't be found on dictated processId\n")
                sys.exit()
            # 校验该节点的下游的对应关系
            if not checkUpstreamOrDwonstreamNodes(data["dispatch"][i]["nodes"][j]["downstream"],
                                                  data, i, j, False):
                sys.stderr.write("Error!!! the dispatch json file ["
                                 + jfile + "]: \n\t "
                                 + "on processor {0}".format(data["dispatch"][i]["rank_id"])
                                 + ": at least one of the downstream nodeId of {0}".format(
                    data["dispatch"][i]["nodes"][j]["node_id"])
                                 + " can't be found on dictated processId \n")
                sys.exit()
    print("check dispatch json file [{0}] successfully!!!!".format(jfile))


def main():
    # 1.命令行参数解析
    parser = OptionParser(usage="%prog [options]")
    parser.add_option("-j", "--jfile", action="store", type="string",
                      dest="jfile", help="the filename of dispatch json file", default='dispatch.json')
    (options, args) = parser.parse_args()

    # 2.校验
    checkDispatchJson(options.jfile)


if __name__ == "__main__":
    main()
