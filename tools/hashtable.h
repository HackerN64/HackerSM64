#pragma once

typedef unsigned int (*HashFunc)(const void *item);
typedef int (*HashValueCmpFunc)(const void *a, const void *b);
struct HashTable;

struct HashTable *hashtable_new(HashFunc func, HashValueCmpFunc cmp, int size, int valueSize);
void hashtable_free(struct HashTable *ht);
void hashtable_insert(struct HashTable *ht, const void *value);
void *hashtable_query(struct HashTable *ht, const void *value);
