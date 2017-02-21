var thub = {

struc: {},
nodeCounter: 0,
courses: {},

init: function(struc) {
  thub.struc = struc;
  thub.initCourseTreeMenus();
},

initCourse: function(courseId, content) {
  var treeId =  "thCourseTree_"+courseId;
  var course = {courseId: courseId, treeId: treeId, content: content};
  thub.courses[courseId] = course;
  thub.initCourseTree(course);
  //$(function(){thub.initCourseVarPar(course)});
},


initCourseTree: function(course) {
  var struc = thub.struc;
  var id = course.treeId;
  var content = course.content;
  var tree = {
    extensions: ["dnd", "table", "gridnav"],
    table: {nodeColumnIdx: 0, indentation: 16},
    // maybe delete gridnav options
    gridnav: {autofocusInput: false, handleCursorKeys: true},


    click: function(e, data) {
        var nodeId = data.node.key;
        Shiny.onInputChange("click", {eventId: "click", id: id, objType: "fancytree_node", nodeId: nodeId, data: data.node.data, nonce: Math.random()});
    },
    renderColumns: function(e, data){
        var node = data.node,
            cols = $(node.tr).find(">td");

        var th = node.data.th;
        var html = "";

        if (th.inputType === "text") {
          value = as_atom(th.value);
          html = '<div class="th_input_div"> <input class="th_input" type="text" name ="'+th.inputId+'" id ="'+th.inputId+'" value="'+value+'"></div>';
        } else if (th.inputType === "textArea") {
          value = String(as_atom(th.value));
          var valueRows = value.split(/\r\n|\r|\n/).length;
          var rows=1;
          if (valueRows > rows) rows=valueRows;

          html = '<div class="th_input_div"> <textarea class="th_input" id ="'+th.inputId+'" rows="'+rows+'">'+value+'</textarea></div>';
        }
        if (typeof th.inputId !== 'undefined') {
          html = html + '<div class="courseTreeMsg_'+th.info.courseId+'" id="'+th.inputId+'__Msg" style="margin:0, padding:0, color: #aa0000;"></div>';
        }

        cols.eq(1).html(html);

    },
    dnd: {
        autoExpandMS: 99999,
        draggable: { // modify default jQuery draggable options
          zIndex: 1000,
          scroll: true,
          containment: "parent",
          revert: "invalid"
        },
        preventRecursiveMoves: true, // Prevent dropping nodes on own descendants
        preventVoidMoves: true, // Prevent dropping nodes 'before self', etc.

        dragStart: function(node, data) {
          // This function MUST be defined to enable dragging for the tree.
          // Return false to cancel dragging of node.
    //    if( data.originalEvent.shiftKey ) ...
    //    if( node.isFolder() ) { return false; }
          return true;
        },
        dragEnter: function(node, data) {
          /* data.otherNode may be null for non-fancytree droppables.
           * Return false to disallow dropping on node. In this case
           * dragOver and dragLeave are not called.
           * Return 'over', 'before, or 'after' to force a hitMode.
           * Return ['before', 'after'] to restrict available hitModes.
           * Any other return value will calc the hitMode from the cursor position.
           */
          // Prevent dropping a parent below another parent (only sort
          // nodes under the same parent):
          if(node.parent !== data.otherNode.parent){
            return false;
          }
          // Don't allow dropping *over* a node (would create a child). Just
          // allow changing the order:
          return ["before", "after"];
          // Accept everything:
          return true;
        },
        dragExpand: function(node, data) {
          // return false to prevent auto-expanding parents on hover
        },
        dragOver: function(node, data) {
        },
        dragLeave: function(node, data) {
        },
        dragStop: function(node, data) {
        },
        dragDrop: function(node, data) {
          // This function MUST be defined to enable dropping of items on the tree.
          // data.hitMode is 'before', 'after', or 'over'.
          // We could for example move the source to the new target:
          data.otherNode.moveTo(node, data.hitMode);
        }
      }

  };

  var nodes = thub.parseNodeFields(1,"course","course",content.course,{courseId: course.courseId});
  tree.source = nodes;
  $("#"+id).fancytree(tree);

  //initCourseTreeMenus();

},


courseContextMenuClick: function(key,opt,nodeType) {
  var node = $.ui.fancytree.getNode(opt.$trigger[0]);
  var th = node.data.th;
  thub.runCourseTreeCommand(key,node,th);
},

runCourseTreeCommand: function(cmd, node,th) {
  if (cmd=="delete") {
    node.remove();
    // send shinyEvent
  }
  thub.initCourseTreeMenus();
},

getAtomNodeInputType: function(st, field, type) {
  if (typeof st !== 'undefined') {
    if (typeof st.inputType !== 'undefined') {
      return(st.inputType);
    }
  }
  if (typeof field !== 'undefined') {
    if (typeof field.inputType !== 'undefined') {
      return(field.inputType);
    }
    if (isArray(field.set)) {
      return("select");
    }
  }
  if (type === "numeric") {
    return("numeric");
  }
  return("text");
},

parseSpecialNode: function(level, parentKey, type, content, field, info) {
  if (type==="varpar") {
    var title= "Variants, Params";
    var varparId =  "thVarPar_"+info.courseId;
    var key= info.courseId+"_varParNode";
    var th = {title: title, special: true, isList: true, isAtom: false, nodeType: type, info: info, fieldName: "varpar"};

    var childth = {special: true, nodeType: "varparTable", info: info,fieldName: "varparTable", inputId: varparId};
    var childnode = {key: info.courseId+"_varParTableNode", title: "", folder: false, extraClasses: "courseNodeType_"+type, children: null, th: childth, icon: ""};

    var node = {key: key, title: title, folder: true, expanded: false, extraClasses: "courseNodeType_"+type, children: [childnode], th: th};
    return(node);
  }
},

parseNode: function(level, parentKey, type, content, field, info) {
  if (level>20) throw("parseNode is nested too deeply!");

  var st = thub.struc[type];
  if (typeof field === "undefined") field = {};
  if (typeof st === "undefined") st = {};

  var title = with_default(field.name,type)+" "+with_default(content.name,"");
  thub.nodeCounter++;
  var key = parentKey + "_" + type + thub.nodeCounter;

  if (field.special === true) {
    return(thub.parseSpecialNode(level, parentKey, type, content, field, info));
  }
  var isList = as_atom(field.isList,false);
  var isAtom = false;
  if (!isList) {
    if (typeof st === 'undefined') {
      isAtom = true;
    } else if (!isArray(st.fields)) {
      isAtom = true;
    }
  }

  var inputId = "input__" + key;
  if (isAtom) {
    inputType = thub.getAtomNodeInputType(st, field, type);
    var choices = with_default(field.set, null);
    var th = {title: title, fieldName: field.name, isList: false, isAtom: true, nodeType: type, inputType: inputType, inputId: inputId, choices: choices, value: content, parentKey: key, info: info};
    var node = {key: key, title: title, folder: false, extraClasses: "courseNodeType_"+type, children: null, th: th};
    return(node);
  }
  var expanded = st.expanded === true | field.expanded === true;

  if (isList) {
    folder = true;
    children = [];
    var childType = field.childType;

    if (isArray(content)) {
      for (var j=0; j<content.length; j++) {
        var child = thub.parseNode(level+1, key, childType,content[j],{}, info);
        children.push(child);
      }
    }
    var th = {title: title, fieldName: field.name, isList: true, isAtom: false, nodeType: type, inputType: "none", inputId: inputId, parentKey: key, value: null, childType: childType, info: info};
    var node = {key: key, title: title, folder: true, expanded: expanded, extraClasses: "courseNodeType_"+type, children: children, th: th};
    return(node);
  }

  // Node with fields
  var children = thub.parseNodeFields(level+1,key,type, content, info);
  var th = {title: title, fieldName: field.name,isList: false,isAtom: false, nodeType: type, inputType: "none", inputId: inputId, parentKey: key, value: null,info: info};
  var node = {key: key, title: title, folder: true, extraClasses: "courseNodeType_"+type, children: children, expanded: expanded, th: th};
  return(node);
},

parseNodeFields: function(level,parentKey, type, content, info) {
  var struc = thub.struc;
  var st = struc[type];
  var fields = st.fields;
  var nodes = [];

  for (var i=0; i<fields.length; i++) {
    var field = fields[i];
    var name = as_atom(field.name);
    var fieldContent = with_default(content[name],"");
    var fieldType = as_atom(field.type, name);
    var node = thub.parseNode(level+1, parentKey, fieldType, fieldContent,field, info);
    nodes.push(node);
  }
  return nodes;
},

initCourseTreeMenus: function() {
  $(function() {
    $.contextMenu({
      selector: '.courseNodeType_stages span.fancytree-title',
      callback: function(key, opt) {
        thub.courseContextMenuClick(key,opt,"stages");
      },
      items: {
        "parse": {name: "Parse Stages"},
        "add": {name: "Add Stage", icon: "edit"},
        "paste": {name: "Paste stage at end", icon: "paste"},
        "sep1": "---",
        "template": {
          "name": "Stage Templates",
        }
      }
    });


    $.contextMenu({
      selector: '.courseNodeType_stage span.fancytree-title',
      callback: function(key, opt) {
        thub.courseContextMenuClick(key,opt,"stage");
      },
      items: {
        "parse": {name: "Parse Stage"},
        "addBefore": {name: "Add Stage Before", icon: "edit"},
        "add": {name: "Add Stage After", icon: "edit"},
        "copy": {name: "Copy Stage", icon: "copy"},
        "paste": {name: "Paste Stage Below", icon: "paste"},
        "delete": {name: "Delete Stage", icon: "delete"},
        "sep1": "---",
        "template": {
          "name": "Stage Templates",
        }
      }
    });


    $.contextMenu({
      selector: '.courseNodeType_actions span.fancytree-title',
      callback: function(key, opt) {
        thub.courseContextMenuClick(key,opt,"actions");
      },
      items: {
        "parse": {name: "Parse Actions"},
        "add": {name: "Add Action", icon: "edit"},
        "copy": {name: "Copy All Actions", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
      }
    });

    $.contextMenu({
      selector: '.courseNodeType_action span.fancytree-title',
      callback: function(key, opt) {
        thub.courseContextMenuClick(key,opt,"action");
      },
      items: {
        "parse": {name: "Parse Action"},
        "add": {name: "Add Action", icon: "edit"},
        "copy": {name: "Copy", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        "delete": {name: "Delete", icon: "delete"},
        "sep1": "---",
        "template": {
          "name": "Action Templates",
          "items": {
              "acceptAction": {"name": "accept"}
          }
        }
      }
    });

    $.contextMenu({
      selector: '.courseNodeType_nature span.fancytree-title',
      callback: function(key, opt) {
        thub.courseContextMenuClick(key,opt,"nature");
      },
      items: {
        "add": {name: "Add Move of Nature", icon: "edit"},
        "copy": {name: "Copy All Moves of Natures", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
      }
    });

    $.contextMenu({
      selector: '.courseNodeType_randomVar span.fancytree-title',
      callback: function(key, opt) {
        thub.courseContextMenuClick(key,opt,"randomVar");
      },
      items: {
        "add": {name: "Add Move of Nature", icon: "edit"},
        "copy": {name: "Copy", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        "delete": {name: "Delete", icon: "delete"},
        "sep1": "---",
        "template": {
          "name": "Templates",
          "items": {
          }
        }
      }
    });

   $.contextMenu({
      selector: '.courseNodeType_compute span.fancytree-title',
      callback: function(key, opt) {
        thub.courseContextMenuClick(key,opt,"compute");
      },
      items: {
        "add": {name: "Add Computation", icon: "edit"},
        "copy": {name: "Copy All Computations", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        "sep1": "---",
        "addPayoffs": {name: "Add Payoffs", icon: "edit"}
      }
    });


    $.contextMenu({
      selector: '.courseNodeType_transformation span.fancytree-title',
      callback: function(key, opt) {
        thub.courseContextMenuClick(key,opt,"action");
      },
      items: {
        "add": {name: "Add Computation", icon: "edit"},
        "copy": {name: "Copy", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        "delete": {name: "Delete", icon: "delete"},
        "sep1": "---",
        "addPayoffs": {name: "Add Payoffs", icon: "edit"}
      }
    });


  });

  return;

},

// show errors messages that R determined when passing course tree
showCourseTreeErrors: function(courseId,log) {
  //alert(JSON.stringify(log))
  var course = thub.courses[courseId];
  var root = $("#"+course.treeId).fancytree("getRootNode");
  // clear old messages
  $(".courseTreeMsg_"+courseId).html("");

  for (key in log) {
    var keyVec = key.split(",");
    var node = thub.findNodeByKeyVec(root,keyVec,true);
    var msgId = node.data.th.inputId + "__Msg";
    $("#"+msgId).html('<p style="color: #a00;">'+log[key]+'</p>');
  }

},

findNodeByKeyVec: function(node, keys, expand) {
  if (expand === true) node.setExpanded(true);
  if (keys.length==0) return(node);
  var key = keys.shift();

  var numKey = parseInt(key);
  var child;
  if (!isNaN(numKey)) {
    child = node.children[numKey-1];
  } else {
    child = thub.findChildNodeByFieldName(node, key);
  }
  return thub.findNodeByKeyVec(child,keys, expand);
},

findChildNodeByFieldName: function(node, fieldName) {
  var children = node.children;
  for (var i = 0; i< children.length; i++) {
    var child = children[i];
    if (child.data.th.fieldName === fieldName) return(child);
  }
  return(null);
}

}; // end thub




function as_atom(value, defaultVal) {
  if (typeof value === 'undefined') return(defaultVal);
  if (value === null) return(null);
  if (value.constructor === Array) return(value[0]);
  return(value);
}

function with_default(value, defaultVal) {
  if (typeof value === 'undefined') return(defaultVal);
  return(value);
}

function isArray(value) {
  if (typeof value === 'undefined') return(false);
  if (value === null) return(false);
  if (value.constructor === Array) return(true);
  return false;
}
