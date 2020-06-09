# Stable Isotope Analysis
![](/home/rob/Code/PythonDataNW/Homework/Final-Project-Isotope-Ratios/Stable-Isotope-Analysis/isotopes.gif)



## Motivation

Stable isotope analysis (SIA) is one of the most exciting developments in forensic chemistry in the 21st century. In addition to the substantial academic hype surrounding SIA, we also find numerous real-world applications including pinning down serial killers, tracing drugs to their distributors, determining the origin of illegal pollutants in protected environments, and enabling archaeologists and paleontologists to better understand environmental conditions from eras long past. Our goal is to be able to create models that leverage this powerful technique to trace where a sample was created, and where it has been.

## How does SIA work
In short, the ratios of isotopes--atoms of various weights with the same atomic number--act as "fingerprints" for geographic locations. By determining the balance, of these isotopes in a sample, we can trace that sample to its geographical origin. These samples can be nearly anything: a lock of hair, a trace of cocaine, or a vial of polluted water. It is to our great benefit that the internet is now flooded with open,free data from global efforts to comprehensively catalog local isotope profiles using advanced infrared spectroscopy.

## Tools used
We have access enormous datapools on the balance isotopes of carbon, oxygen, nitrogen, hydrogen, and a smattering of other trace elements. Much of this data is already abstracted away from raw spectroscopic data into interpretable ratios, which are then zero-centered, normalized, and neatly formatted for our own consumption. We also stand armed with numerous tools, each mature in their own right, yet with their interdisciplinary applications yet unrealized. By using singular value decomposition (the foundation of PCA) to access the full power of linear-algebraic methods, we can determine what parts of these spectroscopic data constitute noise, and what constitute signals. This helps prevent overfitting and improve how well our model generalizes unseen data.

![](/home/rob/Code/PythonDataNW/Homework/Final-Project-Isotope-Ratios/Stable-Isotope-Analysis/map.png)