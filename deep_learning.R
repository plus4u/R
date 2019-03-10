### Deep Learning ###

weight.i = 0.01*matrix(rnorm(layer.size(i)*layer.size(i+1)),
                       nrow = layer.size(i),
                       ncol = layer.size(I+1))
bias.i  = matrix(0, nrow=1, ncol = layer.size(i+1))

# 보통은 가중치와 편차를 하나의 행렬로 묶는다.
 
weight  = 0.01*matrix(rnorm((layer.size(i)+1)*layer.size(i+1)),
                      nrow = layer.size(i)+1,
                      ncol = layer.size(i+1))


# 2X2 차원의 입력값 (Input, 혹은 Feature라고 불린다)
input = matrix(1:4, nrow = 2, ncol = 2)

# 2x3 차원의 가중치
weights = matrix(1:6, nrow = 2, ncol =3)
# 1x3 차원의 오차항
bias = matrix(1:3, nrow = 1, ncol = 3)

# 입력값 x 가중치 + 오차항
input %*% weights + bias

input %*% weights

# 당연히 차원이 맞지 않아 계산이 안 된다

# 해결책 1: 오차항을 두번 반복해서 2X3을 만듦
s1 = input %*% weights + matrix(rep(bias, each=2), ncol=3) 

rep(bias, each=2)

# 해결책 2: sweep 함수 이용
s2 = sweep(input %*% weights, 2, bias, '+')

all.equal(s1, s2)  # [1] TRUE


# Prediction
predict.dnn = function(model, data = X.test) {
    
    # 데이터를 행렬로 변형
    new.data = data.matrix(data)
    
    # sweep을 이용해 hidden layer의 차원을 맞춰줌
    hidden.layer = sweep(new.data %*% model$W1 ,2, model$b1, '+')
    
    # hidden.layer 값 중 최대치를 골라서 score -예측값-를 찾는데 적용
    hidden.layer = pmax(hidden.layer, 0)
    score = sweep(hidden.layer %*% model$W2, 2, model$b2, '+')
    
    # Loss Function: softmax (cross-entropy 라고도 알려져 있다)
    score.exp = exp(score)
    probs = sweep(score.exp, 1, rowSums(score.exp), '/') 
    
    # 가장 확률이 높은 경우를 고른다
    labels.predicted = max.col(probs)
    return(labels.predicted)
}


## train model

train.dnn = function(x, y, traindata=data, testdata=NULL,
                     # hidden layer과 변수의 갯수를 고름 (아래는 1x6으로 지정)
                     hidden = c(6), 
                     
                     # 최대 반복 수치 지정 (클수록 속도를 희생하고 정확성을 끌어올린다)
                     maxit=2000,
                     
                     # delta loss (반복할 때마다 약간씩 수정하는 값의 크기)
                     abstol=1e-2,
                     
                     # learning rate (학습 속도, 작을수록 느리지만 대신 정확하다)
                     lr = 1e-2,
                     
                     # regularization rate (추가적인 feature를 도입하는 속도, 각 feature간 cross-product, 제곱 등이 쓰인다)
                     reg = 1e-3,
                     
                     # 총 100번의 결과값을 보여준다
                     display = 100,
                     random.seed = 1)
{
    # seed를 지정하면 같은 seed 값에서 같은 결과값을 볼 수 있다
    set.seed(random.seed)
    
    # 훈련 집합의 전체 크기
    N = nrow(traindata)
    
    # 데이터 불러오기
    X = unname(data.matrix(traindata[,x]))
    Y = traindata[,y]
    if(is.factor(Y)) { Y = as.integer(Y) }
    
    # 행과 열에 각각 인덱스 생성 
    Y.len   = length(unique(Y))
    Y.set   = sort(unique(Y))
    Y.index = cbind(1:N, match(Y, Y.set))
    
    # input 값들
    D = ncol(X)
    
    # classification이 적용되는 값의 종류 (ex. 0과 1)
    K = length(unique(Y)) # unique(Y) 값이 100개라면 총 100개의 가능성으로 결과값을 보여준다 (사과, 배, 참외 등등등)
    H =  hidden
    
    # 최초 시작점을 정한다 - local max를 찾을 때 최초 시작점을 어떻게 정하느냐가 매우 중요하다. 
    W1 = 0.01*matrix(rnorm(D*H), nrow = D, ncol = H)
    b1 = matrix(0, nrow = 1, ncol = H)
    
    W2 = 0.01*matrix(rnorm(H*K), nrow = H, ncol = K)
    b2 = matrix(0, nrow = 1, ncol = K)
    
    # Training 데이터 전체를 Training에 활용 (N을 그룹으로 쪼개서 bagging을 할 수도 있음)
    batchsize = N
    loss = 100000
    
    # Training 데이터를 NN에 적용
    i = 0
    while(i == maxit && loss > abstol ) {
        
        # iteration index
        i = i +1
        
        # forward 로 계산 (아래에 backward로 계산도 있음)
        # 1은 행, 2는 열
        hidden.layer = sweep(X %*% W1 ,2, b1, '+')
        
        # neurons : ReLU
        hidden.layer = pmax(hidden.layer, 0)
        score = sweep(hidden.layer %*% W2, 2, b2, '+')
        
        # softmax (Loss function을 지정. Cross entropy라고도 불린다)
        score.exp = exp(score)
        probs = sweep(score.exp, 1, rowSums(score.exp), '/') 
        
        # Loss 값을 찾는다 - 매우 작은 값이 나와야 모델이 제대로 "학습"한 것이다
        corect.logprobs = log(probs[Y.index])
        data.loss  = sum(corect.logprobs)/batchsize
        reg.loss   = 0.5*reg* (sum(W1*W1) + sum(W2*W2))
        loss = data.loss + reg.loss
        
        # 결과값을 보여줌
        if( i %% display == 0) {
            if(!is.null(testdata)) {
                model &lt;- list( D = D,
                                  H = H,
                                  K = K,
                                  # 가중치와 오차
                                  W1 = W1, 
                                  b1 = b1, 
                                  W2 = W2, 
                                  b2 = b2)
                labs = predict.dnn(model, testdata[,-y])
                accuracy = mean(as.integer(testdata[,y]) == Y.set[labs])
                cat(i, loss, accuracy, "\n")
            } else {
                cat(i, loss, "\n")
            }
        }
        
        # backward 로 계산
        dscores = probs
        dscores[Y.index] = dscores[Y.index] -1
        dscores = dscores / batchsize
        
        
        dW2 = t(hidden.layer) %*% dscores 
        db2 = colSums(dscores)
        
        dhidden = dscores %*% t(W2)
        dhidden[hidden.layer == 0] = 0
        
        dW1 = t(X) %*% dhidden
        db1 = colSums(dhidden) 
        
        # update ....
        dW2 = dW2 + reg*W2
        dW1 = dW1  + reg*W1
        
        W1 = W1 - lr * dW1
        b1 = b1 - lr * db1
        
        W2 = W2 - lr * dW2
        b2 = b2 - lr * db2
        
    }
    
    # 최종 결과값
    
    model = list( D = D,
                  H = H,
                  K = K,
                  # 가중치와 오차
                  W1= W1, 
                  b1= b1, 
                  W2= W2, 
                  b2= b2)
    
    return(model)
}





## test or validation

# 0. 데이터 불러오기
summary(iris)
plot(iris)

# 1. Test 셋과 Training 셋으로 데이터 분리
samp = c(sample(1:50,25), sample(51:100,25), sample(101:150,25))

# 2. 모델 훈련 시키기
ir.model = train.dnn(x=1:4, y=5, traindata = iris[samp,], testdata = iris[-samp,], hidden=6, maxit=2000, display=50)



# 3. 예측값 계산
labels.dnn = predict.dnn(ir.model, iris[-samp, -5])

# 4. 결과 확인
table(iris[-samp,5], labels.dnn)
# labels.dnn
# 1 2 3
#setosa 25 0 0
#versicolor 0 24 1
#virginica 0 0 25

# 정확성 확인
mean(as.integer(iris[-samp, 5]) == labels.dnn)
# 0.98


# R에서 가장 많이 쓰이는 Neural Network 패키지 중 하나인 nnet와 결과값 비교


library(nnet)
ird = data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                 species = factor(c(rep("s",50), rep("c",50), rep("v",50))))
ir.nn2 = nnet(species ~ ., data = ird, subset = samp, size = 6, rang = 0.1, decay = 1e-2, maxit = 2000)

labels.nnet = predict(ir.nn2, ird[-samp,], type="class")
table(ird$species[-samp], labels.nnet)
# labels.nnet
# c s v
#c 22 0 3
#s 0 25 0
#v 3 0 22

# accuracy
mean(ird$species[-samp] == labels.nnet)
# 0.96










