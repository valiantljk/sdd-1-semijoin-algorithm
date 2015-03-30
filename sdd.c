/*
*This program is an implementation of sdd-1 algorithm
*The course is CS5356 Advanced Database Management System,
*Instructed by Dr.Susan D.Urban
*
*Date   : Mar.31.2012
*Author : Jialin Liu
*Email  : Jaln.Liu@ttu.edu
*Website: http://www.myweb.ttu.edu/jialliu/ 
*/
#include<stdio.h>
#include<stdlib.h>


typedef struct{

int first;
int second;
char attribute;

}RC;

//struct of projection statistics

typedef struct{

int r;
char attribute;
float size;
float SFsj;
}PS;

//struct of semijoin pair
typedef struct{

int first;
int second;
char attribute;
float benefite;
float cost;
}SJ;
//global declare: number of relations, number of connections, and number of statistical tuples.
int numofR,numofC,numofS;
//global declare: number of semijoins,number of best semijoin
int numofsemiJ;
int numofbsemiJ=0;
float *size;
PS findps(int r,char c, PS* ps)
{
        int i=0;
        for(i=0;i<numofS;i++)
        {
                if((ps+i)->r==r&&(ps+i)->attribute==c)
                return *(ps+i);
        }
	//return NULL;
}


//generate the semijoin
void generateSemi(float * size, PS *ps,float ttr,float tmsg, SJ*sj ){

        int i;
        PS tempps;
	//calculate the benefite and cost of all the semijoins in sj
        for (i=0;i<numofsemiJ;i++)
        {
                tempps=findps((sj+i)->second,(sj+i)->attribute,ps);
                (sj+i)->benefite=(1-tempps.SFsj)*(*(size+(sj+i)->first-1))*ttr;
                (sj+i)->cost=tmsg+ttr*(tempps.size);
        }

}

void updateps(int rf,  PS * temp, PS*ps)
{
        int i;
        for(i=0;i<numofS;i++)
        {
		if((ps+i)->r==rf)
                //if((ps+i)->r==rf&&(ps+i)->attribute==temp->attribute)
		{
                (ps+i)->size*=temp->SFsj;
                (ps+i)->SFsj*=temp->SFsj;
		}
        }

}


//update the statistics and semijoin arrays
SJ update(float * size, PS * ps, SJ * sj)
{
        int i=0;
        float max=-10000;
        int tempi=-1;
        SJ *maxsj=(SJ *)malloc(sizeof(SJ));
	//find the best semijoin
        for(i=0;i<numofsemiJ;i++)
        {
                if((sj+i)->benefite>(sj+i)->cost)
                {
                        if ((sj+i)->benefite>max)
                        {
                               *maxsj=*(sj+i);
                                tempi=i;
                                max=(sj+i)->benefite;
                        }
                }
        }

        int j=0;
        //delete the best semijoin from semijoin arrays
        for(j=tempi;j<numofsemiJ-1;j++)
        {
                if((sj+j)!=NULL)
                *(sj+j)=*(sj+j+1);
        }
	//reduce the number of semijoins
	numofsemiJ--;
	//increase the number of best semijoin
	numofbsemiJ++;
	//update information of projection statistics according to the best semijoin
        int rf=maxsj->first;
        int rs=maxsj->second;
        char c=maxsj->attribute;
            
	//update ps and size of rf relation using rs information        
        PS temppss;
        temppss=findps(rs,c,ps);
	//update size of relations
        *(size+rf-1)*=temppss.SFsj;
	for(i=0;i<numofS;i++)
        {
                if((ps+i)->r==rf&&(ps+i)->attribute==c)
		{
                (ps+i)->size*=temppss.SFsj;
                (ps+i)->SFsj*=temppss.SFsj;
		}
		if((ps+i)->r==rf&&(ps+i)->attribute!=c)
		{
		//change the ps according to the updated relation size
		//if the updated Relation size<ps.size, update ps
		if(*(size+rf-1)<(ps+i)->size)
		{
		//(ps+i)->size=*(size+rf-1);
		//claculate the updated ratio
		float updatedratio=*(size+rf-1)/(ps+i)->size;
		 (ps+i)->size=*(size+rf-1);
		//update the ps
		//(ps+i)->SFsj*=updatedratio;
		}
	
		}
        }
       //return the best semijoin
        return *maxsj;

}

void printfeverything( PS *ps, SJ* sj, SJ* ef)
{
        printf("the list of possible semijoins with their cost and benefit:\n");
        int i=0;
        printf("semijoin,       benefit,        cost\n");
        for(i=0;i<numofsemiJ;i++)
        {
                printf("r%d(r%d.%c) SJ r%d(r%d.%c),     %f,     %f\n",(sj+i)->first,(sj+i)->first,(sj+i)->attribute
                        ,(sj+i)->second,(sj+i)->second,(sj+i)->attribute,(sj+i)->benefite,(sj+i)->cost);
        }
	if(numofbsemiJ>0)
	{
        printf("the most beneficial semijoins with their cost and benefit:\n");
	printf("semijoin,       benefit,        cost\n");
	
	for(i=0;i<numofbsemiJ;i++)
	{
		printf("r%d(r%d.%c) SJ r%d(r%d.%c),     %f,     %f\n",(ef+i)->first,(ef+i)->first,(ef+i)->attribute
                        ,(ef+i)->second,(ef+i)->second,(ef+i)->attribute,(ef+i)->benefite,(ef+i)->cost);
	}
	}
        
        printf("the changes of statistics caused by execution of the most beneficial semijoin:\n");
        printf("projection,     size,   SFsj\n");
        for(i=0;i<numofS;i++)
        {
                printf("r%d.%c, %f,     %f\n",(ps+i)->r,(ps+i)->attribute,(ps+i)->size,(ps+i)->SFsj);
        }
	printf("the changes of statistics of realtions caused by execution of the most beneficial semijoin\n");
	printf("relation,	size\n");
	for(i=0;i<numofR;i++)
	{
		printf("r%d,	%f\n",i+1,*(size+i));
	}

}



int main()
{
	int i=0;
	//char * relations[];
	float ttr,tmsg;
	//float * size;
	RC * rc;
	SJ * sj;
	PS * ps;
	int flag=0;
	/************************input*************************/
	
	//number of relations
	printf("please input the number of relations:\n");
	scanf("%d",&numofR);

	//allocate space for relations
	size=(float *)malloc(numofR*sizeof(float));
	//relations size
	printf("please input the relation size\n");
	while(i<numofR)
	{
		printf("\nR%d:",i+1);
		scanf("%f",size+i);
		i++;
	}
	//message cost and transmission cost
	printf("please input the message cost:\n");
	scanf("%f",&tmsg);
	printf("please input the transmission cost:\n");
	scanf("%f",&ttr);
	//number of connection
	printf("please input the number of connections between relations:\n");
	scanf("%d",&numofC);
	rc=(RC *)malloc(numofC*sizeof(RC));
        if(rc==NULL)
	printf("malloc error\n");
	i=0;
	//connection between realtions
	while(i<numofC)
	{
		printf("\ninput the %d connection:(eg.1 2 A)\n", i+1);
		scanf("%d%d",&((rc+i)->first),&((rc+i)->second));
		getchar();
		(rc+i)->attribute=getchar();;
		i++;
	}
	
	//initial projection statistics
	printf("please input the number of statistics tuples\n");
	scanf("%d",&numofS);
	ps=(PS *)malloc(numofS*sizeof(PS));
	printf("please input the initial projection statistics\ne.g:1 36 0.3 A\n");
	//printf("relation	attribute	size	selection factor\n");
	for ( i=0;i<numofS;i++)
	{
	        printf("relation       size    selection_factor		attribute\n");
		scanf("%d%f%f",&((ps+i)->r),&((ps+i)->size),&((ps+i)->SFsj));
		getchar();
		(ps+i)->attribute=getchar();
	}
	/*************************END OF INPUT*******************************/


	/***********************SDD-1 ALGORITHM******************************/
	//int num=numofC*2;
	numofsemiJ=numofC*2;
	sj=(SJ *)malloc(numofsemiJ*sizeof(SJ));
	//possible semijoins
	for (i=0;i<numofsemiJ;i++)
	{
		
		if(i<numofC){
                (sj+i)->first=(rc+i)->first;
                (sj+i)->second=(rc+i)->second;
                (sj+i)->attribute=(rc+i)->attribute;

                }
                else{
                (sj+i)->first=(rc+i-numofC)->second;
                (sj+i)->second=(rc+i-numofC)->first;
                (sj+i)->attribute=(rc+i-numofC)->attribute;
                }

	}
	SJ * ef;
	ef=(SJ *)malloc(numofsemiJ*sizeof(SJ));
	
	//while(num>0&&sj!=NULL&&sj->attribute>0)
	//in each loop, there will be only one semijoin being eliminated(how if two equal semijoins exist)
	int totalloops=numofsemiJ;
	for(i=0;i<totalloops;i++)
	{
		//generarte statistics of semijoins, like benefite,cost
		generateSemi(size,ps,ttr,tmsg,sj);
		//print everything during each loop.
		printfeverything(ps,sj,ef);
		//update the statistics information,save the most benefitial semijoin into ef.
		*(ef+i)=update(size,ps,sj);
		
	}
	printf("********************************************\n");
	//find the assembly site for the query
	float sizetemp=*size;
	int assemblysite=0;	
	for(i=1;i<numofR;i++)
	{
		if(sizetemp<*(size+i))
		{
			assemblysite=i;
		}
	}	
	printf("the assembly size for the query is:\n r%d \n",assemblysite+1);		
	return 0;
}

