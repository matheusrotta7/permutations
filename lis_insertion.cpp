// C++ program to get minimum number of insertion
// steps to sort an array
#include <bits/stdc++.h>
using namespace std;

void print_lis(int max_index, int arr[], int N, int pred[]) {
    int cur_index = max_index;
    stack<int> lis_stack;

    while (pred[cur_index] != -1) {
        lis_stack.push(arr[cur_index]);
        cur_index = pred[cur_index];
    }
    lis_stack.push(arr[cur_index]);
    cout << "LIS: ";
    while (!lis_stack.empty()) {
     cout << ' ' << lis_stack.top();
     lis_stack.pop();
    }
    cout << endl;
}

// method returns min steps of insertion we need
// to perform to sort array 'arr'
int minInsertionStepToSortArray(int arr[], int N)
{
	// lis[i] is going to store length of lis
	// that ends with i.
	int lis[N];
    int pred[N];

	/* Initialize lis values for all indexes */
	for (int i = 0; i < N; i++) {
        lis[i] = 1;
        pred[i] = -1;
    }

	/* Compute optimized lis values in bottom up manner */
	for (int i = 1; i < N; i++)
		for (int j = 0; j < i; j++)
			if (arr[i] >= arr[j] && lis[i] < lis[j] + 1) {
                lis[i] = lis[j] + 1;
                pred[i] = j;
            }

	/* The overall LIS must end with of the array
	elements. Pick maximum of all lis values */
	int max = 0;
    int max_index;
	for (int i = 0; i < N; i++) {
        if (max < lis[i]) {
            max = lis[i];
            max_index = i;
        }
    }

	// return size of array minus length of LIS
	// as final result
    print_lis(max_index, arr, N, pred);
	return (N - max);
}

// Driver code to test above methods
int main()
{
	int arr[] = {2, 3, 5, 1, 4, 7, 6}; //1 2 3 4 5 6 7
	int N = sizeof(arr) / sizeof(arr[0]);
	minInsertionStepToSortArray(arr, N);
	return 0;
}
