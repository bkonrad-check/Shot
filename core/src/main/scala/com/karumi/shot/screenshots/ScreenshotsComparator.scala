package com.karumi.shot.screenshots

import com.karumi.shot.domain.{Config, DifferentImageDimensions, DifferentScreenshots, Dimension, Screenshot, ScreenshotComparisonError, ScreenshotNotFound, ScreenshotsComparisionResult}
import com.karumi.shot.domain.model.ScreenshotsSuite
import com.sksamuel.scrimage.Image

import java.nio.file.{Files, Path, Paths}
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import scala.util.Try

class ScreenshotsComparator {

  def compare(screenshots: ScreenshotsSuite, tolerance: Double): ScreenshotsComparisionResult = {
    val errors = screenshots.par.flatMap(compareScreenshot(_, tolerance)).toList
    ScreenshotsComparisionResult(errors, screenshots)
  }

  private def compareScreenshot(screenshot: Screenshot, tolerance: Double): Option[ScreenshotComparisonError] = {
    val recordedScreenshotPath = Paths.get(screenshot.recordedScreenshotPath)
    if (!Files.exists(recordedScreenshotPath)) {
      Some(ScreenshotNotFound(screenshot))
    } else {
      val oldScreenshot = loadImage(recordedScreenshotPath)
      val newScreenshot = ScreenshotComposer.composeNewScreenshot(screenshot)
      if (!haveSameDimensions(toBufferedImage(newScreenshot), oldScreenshot)) {
        val originalDimension = Dimension(oldScreenshot.getWidth, oldScreenshot.getHeight)
        val newDimension = Dimension(newScreenshot.width, newScreenshot.height)
        Some(DifferentImageDimensions(screenshot, originalDimension, newDimension))
      } else if (imagesAreDifferent(screenshot, oldScreenshot, toBufferedImage(newScreenshot), tolerance)) {
        Some(DifferentScreenshots(screenshot))
      } else {
        None
      }
    }
  }

  private def toBufferedImage(image: Image): BufferedImage = {
    val width = image.width
    val height = image.height
    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g2d = bufferedImage.createGraphics()
    Try(g2d.drawImage(image.awt, 0, 0, null)).getOrElse(g2d.dispose())
    bufferedImage
  }

  private def loadImage(path: Path): BufferedImage = {
    ImageIO.read(path.toFile)
  }

  private def imagesAreDifferent(
                                  screenshot: Screenshot,
                                  oldScreenshot: BufferedImage,
                                  newScreenshot: BufferedImage,
                                  tolerance: Double
                                ) = {
    if (oldScreenshot == newScreenshot) {
      false
    } else {
      val oldScreenshotPixels = oldScreenshot.getRGB(0, 0, oldScreenshot.getWidth, oldScreenshot.getHeight, null, 0, oldScreenshot.getWidth)
      val newScreenshotPixels = newScreenshot.getRGB(0, 0, newScreenshot.getWidth, newScreenshot.getHeight, null, 0, newScreenshot.getWidth)

      val differentPixels = oldScreenshotPixels.zip(newScreenshotPixels).filter { case (a, b) => a != b }
      val percentageOfDifferentPixels = differentPixels.length.toDouble / oldScreenshotPixels.length.toDouble
      val percentageOutOf100 = percentageOfDifferentPixels * 100.0
      val imagesAreDifferent = percentageOutOf100 > tolerance
      val imagesAreConsideredEquals = !imagesAreDifferent
      if (imagesAreConsideredEquals && tolerance != Config.defaultTolerance) {
        val screenshotName = screenshot.name
        println(
          Console.YELLOW + s"⚠️   Shot warning: There are some pixels changed in the screenshot named $screenshotName, but we consider the comparison correct because tolerance is configured to $tolerance % and the percentage of different pixels is $percentageOutOf100 %" + Console.RESET
        )
      }
      imagesAreDifferent
    }
  }

  private def haveSameDimensions(newScreenshot: BufferedImage, recordedScreenshot: BufferedImage): Boolean =
    newScreenshot.getWidth == recordedScreenshot.getWidth && newScreenshot.getHeight == recordedScreenshot.getHeight

}
