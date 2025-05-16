#ifndef AST_H
#define AST_H

typedef enum NodeType {
    NODE_UNKNOWN,
} NodeType;

typedef struct NodeBase {
    NodeType type;
} NodeBase;

typedef union Node {
    NodeType type;
    NodeBase base;
} Node;




#endif /* AST_H */
