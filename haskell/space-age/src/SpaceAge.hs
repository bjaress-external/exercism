module SpaceAge (Planet(..), ageOn) where

data Planet = Earth
            | Jupiter
            | Mars
            | Mercury
            | Neptune
            | Saturn
            | Uranus
            | Venus
yearRatio Earth = 1.0
yearRatio Jupiter = 11.862615
yearRatio Mars = 1.8808158
yearRatio Mercury = 0.2408467
yearRatio Neptune = 164.79132
yearRatio Saturn = 29.447498
yearRatio Uranus = 84.016846
yearRatio Venus = 0.61519726

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / secondsPerEarthYear / (earthYearsPerYear planet)
    where
    earthYearsPerYear = yearRatio
    secondsPerEarthYear = 31557600