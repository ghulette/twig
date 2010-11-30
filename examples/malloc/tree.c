#include <stdio.h>
#include <stdlib.h>

struct node_s {
  int n;
  struct node_s *left;
  struct node_s *right;
};

typedef struct node_s Node;

Node *new_node(int x) {
  Node *nd = (Node *)malloc(sizeof(Node));
  nd->n = x;
  nd->left = NULL;
  nd->right = NULL;
  return nd;
}

void delete_node(Node *nd) {
  if(nd->left != NULL) {
    delete_node(nd->left);
  }
  if(nd->right != NULL) {
    delete_node(nd->right);
  }
  free(nd);
}

void insert(int x, Node *nd) {
  if(x < nd->n) {
    if(nd->left == NULL) {
      nd->left = new_node(x);
    }
    else {
      insert(x,nd->left);
    }
  }
  else {
    if(nd->right == NULL) {
      nd->right = new_node(x);
    }
    else {
      insert(x,nd->right);
    }
  }
}

int contains(int x, Node *nd) {
  if(x == nd->n) {
    return 1;
  }
  else if(x < nd->n) {
    if(nd->left == NULL) {
      return 0;
    }
    else {
      return contains(x,nd->left);
    }
  }
  else {
    if(nd->right == NULL) {
      return 0;
    }
    else {
      return contains(x,nd->right);
    }
  }
}

int main() {
  Node *root = new_node(0);
  int i;
  for(i=0; i<100; i+=13) {
    printf("insert %d\n",i%17);
    insert(i%17,root);
  }
  for(i=0; i<17; i++) {
    printf("Contains %02d? %d\n", i, contains(i,root));
  }
  delete_node(root);
  return 0;
}
