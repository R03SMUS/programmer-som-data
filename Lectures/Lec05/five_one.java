import java.util.Arrays;

public class five_one {
    public static void main(String[] args) {
        int[] l1 = {1,2,4};
        int[] l2 = {3,6,7};
        int[] l3 = merge(l1, l2);
        System.out.println(Arrays.toString(l3));
    }
    
    public static int[] merge(int[] l1, int[] l2) {
        int length = l1.length + l2.length;
        int[] i = new int[length];
        for  (int j = 0; j < l1.length; j++) {
            i[j] = l1[j];
        }
        for (int k = 0; k < l2.length; k++) {
            i[k+l1.length] = l2[k];
        }
        Arrays.sort(i);
        return i;
    }
}