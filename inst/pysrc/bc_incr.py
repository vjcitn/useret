import sklearn
import numpy as np

from sklearn.decomposition import PCA, IncrementalPCA

def bc_ipca( mat, ncomp, chsize ):
   ipca = IncrementalPCA(n_components=ncomp, batch_size=chsize)
   return (ipca, ipca.fit_transform(mat))

def bc_pca( mat, ncomp ):
   pca = PCA(n_components=ncomp)
   return (pca, pca.fit_transform(mat))

def bc_chunked_ipca( mat, chunk_size, ncomp, bsize ):
   n = mat.shape[0]
   ans = mat.copy()
   ipca = IncrementalPCA(n_components=ncomp, batch_size=bsize)
   for i in range(0, (n//chunk_size)):
     ipca.partial_fit(mat[i*chunk_size : (i+1)*chunk_size])
   for i in range(0, (n//chunk_size)):
     ans[i*chunk_size : (i+1)*chunk_size] = ipca.fit_transform(mat[i*chunk_size : (i+1)*chunk_size])
# need a finishing step
   return (ipca, ans)
