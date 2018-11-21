object Driver{

  def main(args:Array[String]){
    val lines = sumCubes(1,3)
    //println(lines)
    val x = List(List(1.0,2.0,3.0,4.0,7.0,9.0),List(5.0,6.0,7.0,8.0,30.0,21.0),List(9.0,10.0,11.0,12.0,20.0,21.0),List(13.0,14.0,15.0,16.0,17.0,18.0),List(1.0,2.0,3.0,4.0,7.0,9.0),List(1.0,2.0,3.0,4.0,7.0,9.0))
    //println(PoolingLayer(max,x,3))
    //println(multijoin(List(List(1,2,3,4),List(5,6,7,8)),Nil,2,0))
    //println(ActualPooling(max,x,2))
    //println(addMatrix(List(List(1.0,2.0,3.0,4.0,7.0,9.0),List(5.0,6.0,7.0,8.0,30.0,21.0)),
      //  List(List(9.0,10.0,11.0,12.0,20.0,21.0),List(13.0,14.0,15.0,16.0,17.0,18.0)),Nil))
    
    println(avg
        (List(1.0,0.0,2.0,3.0,4.0)))

}
def sum(xs: List[Double]): Double = {
    if(xs == Nil)
      0
    else
      xs.head + sum(xs.tail)
  }
  def avg(xs: List[Double]): Double = {
    sum(xs)/(xs.size)
  }

       def convolute(Image:List[List[Double]], Kernel:List[List[Double]],ImageSize : List[Int],KernelSize:List[Int]):List[List[Double]] = {
    val n1 = ImageSize.head
    val m1 = ImageSize.tail.head
    val n2 = KernelSize.head
    val m2 = KernelSize.tail.head
    if(n1 < n2)Nil
    else
    {
    def removeColumn(m:List[List[Double]]):List[List[Double]] = {
      if(m==Nil)
        Nil
      else
      {
        m.head.tail :: removeColumn(m.tail)
      }
    }

    def rowOfAns(Image:List[List[Double]], Kernel:List[List[Double]], colnum:Int):List[Double] = {
      
      if(colnum+m2 <= m1)
      {
        dotProduct(Kernel,Image) :: rowOfAns(removeColumn(Image),Kernel,colnum+1) 
      }
      else
      Nil
    }
    rowOfAns(Image,Kernel,0)::convolute(Image.tail,Kernel,List(n1-1,m1),KernelSize)
    
    }
    
    
  }


def cube(x: Int): Int = x * x * x
def sumCubes(a: Int, b: Int): Int =
if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

   def flatten[A](list: List[List[A]]):List[A] = {
    if (list.length==0) List[A]() 
    else list.head ++ flatten(list.tail)
}
   def min(xs: List[Double]): Double = {
    if(xs.tail.nonEmpty){
      val tl = min(xs.tail)
      if(tl < xs.head) tl
      else xs.head
    }else{
      xs.head
    }
}
    def max(xs: List[Double]): Double = {
    if(xs.tail.nonEmpty){
      val tl = max(xs.tail)
      if(tl > xs.head) tl
      else xs.head
    }else{
      xs.head
    }
  }
def split(in:List[List[Double]],size:Int): List[List[Double]] = {
       if(in==Nil)
         Nil
       else
       {
        if(size == 0)
        in
        else
        {
           if(in.tail == Nil)
           {
            split(List(List(in.head.head),in.head.tail),size-1)
           }
           else
            split(List(in.head:::List(in.tail.head.head),flatten(in.tail).tail),size-1)
       }
       }
   }   
def multisplit(in:List[List[Double]],out:List[List[Double]],size:Int): List[List[Double]] = {
    if(in==List(List()))
      out
    else
    {
      val s=split(in,size)
      multisplit(s.tail,out:::List(s.head),size)
    }
  }

   def index(l:List[Double],i:Int,count:Int): Double ={
       if(l==Nil)
       Nil
       if(count==i)
       l.head
       else
       index(l.tail,i,count+1)
   }
   
   def index2d(l:List[List[Double]],i:Int,j:Int,counti:Int,countj:Int): Double ={
       if(counti==i)
       {
           if(countj ==j)
           l.head.head
           else
           index(l.tail.head,j,countj+1)
       }
       else
       index2d(l.tail,i,j,counti+1,countj)
   }
   


//Convolution Layer 1
    def dotProduct(a:List[List[Double]],b:List[List[Double]]):Double ={
       if(a == Nil)
       0
       else
       scalarMultiply(b.head,a.head)+dotProduct(a.tail,b.tail)
       
   }
   def scalarMultiply(a:List[Double],b:List[Double]):Double ={
       if(a == Nil)
       0
       else
       b.head*a.head+scalarMultiply(a.tail,b.tail)
       
   }
//Pooling Layer
  
  def join(l1:List[List[Double]],l2:List[List[Double]],l3:List[List[Double]]):List[List[Double]]= {
      if(l1==Nil && l2!=Nil)
      l2
      else if(l2==Nil && l1!=Nil)
      l1
      else if(l1==Nil && l2==Nil)
      l3
      else
      join(l1.tail,l2.tail,l3:::(List(l1.head:::l2.head)))
  }
  
  def multijoin(m:List[List[Double]],out:List[List[Double]],size:Int,count:Int):List[List[Double]] ={
      if(count==size)
      out
      else if(m==Nil)
        out
      else
      {
          val ms=multisplit(List(m.head),Nil,size)
          multijoin(m.tail,join(out,ms,Nil),size,count+1)
      }
      
  }
    def activatepool(poolingFunc:List[Double]=>Double,m:List[List[Double]],out:List[Double]):List[Double] ={
        if(m==Nil)
            out
        else
            activatepool(poolingFunc,m.tail,out:::List(poolingFunc(m.head)))

    }

     def singlepooling(poolingFunc:List[Double]=>Double,Image:List[List[Double]],out:List[Double], size:Int):List[Double] = {
       if(Image==Nil)
       out
       else if(Image.head.size == size)
        List(poolingFunc(flatten(Image)))
       else
       {
       val mj = multijoin(Image,Nil,size,0)
       activatepool(poolingFunc,mj,Nil)
       }
   }
      def singlePooling(poolingFunc:List[Double]=>Double,Image:List[List[Double]], size:Int):List[Double] = {
        singlepooling(poolingFunc,Image,Nil,size)
      }

      def Pooling(poolingFunc:List[Double]=>Double,Image:List[List[Double]],out:
    List[List[Double]],temp:List[List[Double]],size:Int,count:Int):
    List[List[Double]]={ 
        if(Image == Nil && temp==Nil)
            out
        
        else
        {
        if(count==size) 
        {
            Pooling(poolingFunc,Image,out:::List(singlePooling(poolingFunc,temp,size)),Nil,size,0) 
        }
        else
            Pooling(poolingFunc,Image.tail,out,temp:::List(Image.head),size,count+1)
        }

    }

    def PoolingLayer(poolingFunc:List[Double]=>Double,Image:List[List[Double]],size:Int): List[List[Double]] = {
        Pooling(poolingFunc,Image,Nil,Nil,size,0)
    }



// Activation Layer   
def activationHelper(f: Double => Double, m: List[Double],out:List[Double]): List[Double] =
     {
         if(m==Nil)
         out
         else
         activationHelper(f,m.tail,out:::List(f(m.head)))
     }
     
     def activation(f: Double => Double, m: List[List[Double]],out: List[List[Double]]): List[List[Double]] =
     {
         if(m==Nil)
         out
         else
         {
             activation(f,m.tail,out:::List(activationHelper(f,m.head,Nil)))
         }
     }

     def activationLayer(f: Double => Double, m: List[List[Double]]): List[List[Double]] =
     {
         activation(f,m,Nil)
     }
//Normalization
   def normalizehelper(m:List[Double],out:List[Int],maxi:Double,mini:Double):List[Int] = {
       if(m==Nil)
       out
       else
       normalizehelper(m.tail,out:::List((scala.math.round((m.head-mini)/(maxi-mini)*255)).toInt),maxi,mini)
   }
   
   def normalize(m:List[List[Double]],out:List[List[Int]],maximum:Double,minimum:Double): List[List[Int]] = {
        if(m==Nil)
        out
        else
        {            
        normalize(m.tail,out:::List((normalizehelper(m.head,Nil,maximum,minimum))),maximum,minimum)
        }
      }
    def normalise(m:List[List[Double]]): List[List[Int]] = {
      val maximum = max(flatten(m))
      val minimum = min(flatten(m))
      normalize(m,Nil,maximum,minimum)
    }

    def mixedLayer(Image:List[List[Double]],Kernel:List[List[Double]],imageSize:List[Int],kernelSize:List[Int],
        activationFunc:Double => Double,poolingFunc:List[Double]=>Double,K:Int): List[List[Double]] ={

        PoolingLayer(poolingFunc, activationLayer(activationFunc,convolute(Image,Kernel,imageSize,kernelSize)),K)
    }



def addMatrix(l1:List[List[Double]],l2:List[List[Double]],l3:List[List[Double]]):List[List[Double]]= {
      if(l1==Nil && l2!=Nil)
      l2
      else if(l2==Nil && l1!=Nil)
      l1
      else if(l1==Nil && l2==Nil)
      l3
      else
      addMatrix(l1.tail,l2.tail,l3:::(List(listAdd(l1.head,l2.head))))  
  }

  def listAdd(l1:List[Double],l2:List[Double]):List[Double]= {
    if(l1==Nil || l2 == Nil)
      return Nil
    (l1.head+l2.head)::listAdd(l1.tail,l2.tail)
  }


    def assembly(Image:List[List[Double]],imageSize:List[Int],w1:Double,w2:Double,b:Double,Kerne11:List[List[Double]],kernelSize1:List[Int],Kernelist2:List[List[Double]],kernelSize2:List[Int],Kernelist3:List[List[Double]],kernelSize3:List[Int],Size: Int):List[List[Int]]={
      val temporary_1=mixedLayer(Image,Kerne11,imageSize,kernelSize1,(x:Double)=>if(x>0) x else 0,avg,Size)
      val temporary_2=mixedLayer(Image,Kernelist2,imageSize,kernelSize2,(x:Double)=>if(x>0) x else 0,avg,Size)

 val a1=activationLayer((x:Double)=>x*w1,temporary_1)
 val a2=activationLayer((x:Double)=>x*w2,temporary_2)
 val a3=addMatrix(a1,a2,Nil)

 val t3=activationLayer((x:Double)=>x+b,a3)
 
 val r= (imageSize.head - kernelSize1.head + 1)/Size
 val c= (imageSize.tail.head - kernelSize1.tail.head + 1)/Size
 val sa3= List(r,c)

 val t4=mixedLayer(t3,Kernelist3,sa3,kernelSize3,(x:Double)=>if(x>0)x else 0.5*x,max,Size)
 normalise(t4)
}
}