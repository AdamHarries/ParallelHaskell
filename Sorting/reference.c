typedef int elem_t;

void swap(elem_t* a, elem_t* b){
    elem_t temp;
    temp = *b;
    *b = *a;
    *a = temp;
}

// __device__ void d_swap_agents(g_agent *a, g_agent *b)
// {
//     int temp;
//     for(int i = 0;i<CITY_COUNT;i++)
//     {
//         temp = a->order[i];
//         a->order[i] = b->order[i];
//         b->order[i] = temp;
//     }
//     temp = a->length;
//     a->length = b->length;
//     b->length = temp;
// }

void do_swaps(elem_t* arr, int n, int sls, int scl, int step_size)
{
    for(int i = 0;i<n/2;i++){
        // index of comparison (i.e. which of half comparison lines)
        int cid = i;
        // index of first comparison element
        int aid = ((cid*2)-(cid%step_size))
        // the direction of comparison (up, or down?)
        int d = ((cid / (sls/2)) % slc)%2;
        // compare and swap (assume a comparison operator)
        if((arr[aid] < arr[aid + step_size]) == d){
            swap(&(arr[aid]), &(arr[aid+step_size]));
        }
    }
}

// __global__ void swap_agents(g_agent *arr, int sls, int slc, int step_size)
// {
//     //get index of comparison (ie, we always have len(arr) comparisons, which one are we?)
//     int cid = get_tid();
//     //get index of first comparison element
//     int aid = ((cid*2)-(cid%(step_size)));
//     //get direction of comparison
//     int d = ((cid/(sls/2))%slc)%2;
//     //compare and swap:
//     if((arr[aid].length<arr[aid+step_size]).length==d)
//     {
//         d_swap_agents(&(arr[aid]), &(arr[aid+step_size]));
//     }
// }

//in place, uses bitonic sort
void bitonic_sort(g_agent* a_list, int agent_count)
{
    int sublist_size, swap_dist;
    for(sublist_size=2;sublist_size<=n; sublist_size*=2)
    {
        //loop over swap distances
        //aka, merge up subhypercubes, pseudo-recursively
        for(swap_dist=sublist_size/2;swap_dist>=1;swap_dist=swap_dist/2)
        {
            do_swaps(arr, agent_count, sublist_size, n/sublist_size, swap_dist);
            //launch swap kernel
            swap_agents<<<1,n/2>>>(a_list, sublist_size,n/sublist_size, swap_dist);
            //synchronise threads ready to launch again
            cudaError_t err = cudaThreadSynchronize();
            //get errors with synchronisation
            if( err != cudaSuccess)
                printf("cudaThreadSynchronize error: %s\n", cudaGetErrorString(err));   
        }       
    }
}