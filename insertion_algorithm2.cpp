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

int get_highest_offset_index(vector<int> a, int n) {
    int highest_val = -1;
    int highest_ind = -1;
    for (int i = 1; i <= n; i++) {
        if (abs(a[i]) > highest_val) {
            highest_val = abs(a[i]);
            highest_ind = i;
        }
    }
    return highest_ind;
}

void fix_hoi(int hoi, vector<int> &aux, int n) {
    if (aux[hoi] > 0) {
        //then our element wants to go to the right
        int offset = aux[hoi];
        int target_ind = hoi + offset;
        //now we must increment all the elements in the interval because they are going one position to the left
        for (int i = hoi; i <= target_ind-1; i++) {
            aux[i] = aux[i+1] + 1; //we are also shifting them leftwards
        }
        aux[target_ind] = 0; //we are considering we just put the element in the correct position
    }
    else if (aux[hoi] < 0) {
        int offset = aux[hoi];
        int target_ind = hoi + offset;
        //now we must decrement all the elements in the interval because they are going one position to the right
        for (int i = hoi; i >= target_ind + 1; i--) {
            aux[i] = aux[i-1] - 1;
        }
        aux[target_ind] = 0;
    }
    else {
        cout << "Unexpected behavior" << endl;
    }
}

int sort_via_insertions(vector<int> a, int n) {
    // bool sortable = true
    // display(a, n);
    vector<int> aux = vector<int>(n+1);
    int insertions = 0;
    //create index difference
    for (int i = 1; i <= n; i++) {
        aux[i] = a[i]-i;
    }

    for (int i = 1; i <= n; ) {
        int lc, rc, lc_value, rc_value, rc_index; //left candidate, right candidate (to be inserted correctly)
        lc = i; //left candidate is always the current value
        if (aux[lc] == 0) {
            //then the element is already in the correct position, just continue and don't count it as an insertion
            i++;
            continue;
        }
        else {
            lc_value = abs(aux[lc]);
            //now we must discover the value that would like to come to the current position:
            int desired_offset = -1; //the offset that makes the element go to the correct position
            for (int j = i + 1; j <= n; j++) {
                if (aux[j] ==  desired_offset--) {
                    rc_value = abs(aux[j]);
                    rc_index = j;
                    break;
                }
            } //should always be able to find a value (verify)
            if (lc_value > rc_value) {
                fix_hoi(i, aux, n);
            }
            else if (rc_value > lc_value) {
                fix_hoi(rc_index, aux, n);
                i++; //increment only when you pick the right-most candidate
            }
            else {
                //if they are equal, pick right value
                fix_hoi(rc_index, aux, n);
                i++;
            }
            insertions++;
        }
    }

    cout << "offset vector at the end: " << endl;
    display(aux, n);
    return insertions;
}

// Function to find the permutations (adapted from geeksandgeeks)
void count_sortable_permutations(vector<int> a, int n)
{
    int count = 0;
    // Sort the given array
    // sort(a, a + n);

    // Find all possible permutations
    // cout << "Possible permutations are:\n";
    // do {
    //4  1  5  6  2  3
    a[1] = 4;
    a[2] = 1;
    a[3] = 5;
    a[4] = 6;
    a[5] = 2;
    a[6] = 3;
        cout << endl << endl;
        cout << "initial vector: " << endl;
        display(a, n);
        int insertions = sort_via_insertions(a, n);
        cout << "number of necessary insertions: "<< insertions << endl;
    // } while (next_permutation(a.begin() + 1, a.end()));

}

// Driver code
int main()
{

    // int a[] = { 10, 20, 30, 40 };
    for (int n = 6; n <= 6; n++) {

        int vsize = n+1;
        vector<int> a(vsize); //ignore first index 0 to make code more readable, vector is one size bigger than necessary
        for (int i = 1; i <= n; i++) {
            a[i] = i;
        }

        // int n = sizeof(a) / sizeof(a[0]);

        count_sortable_permutations(a, n);
    }
    // int n = 3;

    return 0;
}
