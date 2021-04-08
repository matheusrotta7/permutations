#include <bits/stdc++.h>
using namespace std;

// Function to display the array
void display(vector<int> a, int n)
{
    for (int i = 1; i <= n; i++) {
        cout << a[i] << "  ";
    }
    cout << endl;
}

bool check_if_sortable_via_fixed_switches(vector<int> a, int n) {
    // bool sortable = true;
    for (int i = 1; i <= n; i++) {
        if (a[i] != i) {
            if (a[a[i]] != i) {
                return false;
            }
        }
    }
    return true;
}

// Function to find the permutations (adapted from geeksandgeeks)
int count_sortable_permutations(vector<int> a, int n)
{
    int count = 0;
    // Sort the given array
    // sort(a, a + n);

    // Find all possible permutations
    // cout << "Possible permutations are:\n";
    do {
        // display(a, n);
        bool sortable = check_if_sortable_via_fixed_switches(a, n);
        if (sortable)
            count++;
    } while (next_permutation(a.begin() + 1, a.end()));
    return count;
}

// Driver code
int main()
{

    // int a[] = { 10, 20, 30, 40 };
    for (int n = 2; n <= 11; n++) {

        int vsize = n+1;
        vector<int> a(vsize); //ignore first index 0 to make code more readable, vector is one size bigger than necessary
        for (int i = 1; i <= n; i++) {
            a[i] = i;
        }

        // int n = sizeof(a) / sizeof(a[0]);

        cout << "The number of sortable permutations for n = " << n << " is: " << count_sortable_permutations(a, n) << endl;
    }
    // int n = 3;

    return 0;
}
