import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Scanner;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;
import java.awt.Graphics2D;
import java.awt.Font;
import java.awt.Color;

public class ScreenNumberWriter {

	public static void main(String[] args) {
		if (args.length < 3) {
			System.out.println("Error: not enough arguments");
			return;
		}
		String infilename = args[0];
		String numbersfilename = args[1];
		String outfilename = args[2];
		BufferedImage mapImage = null;
		try {
			mapImage = ImageIO.read(new File(infilename));
		} catch (IOException e) {
			System.out.println("Error: failed to load input image");
			return;
		}
		Scanner s = null;
		try {
			s = new Scanner(new File(numbersfilename));
		} catch (FileNotFoundException e) {
			System.out.println("Error: numbers file not found");
			return;
		}
		int tileWidth = 16;
		int scaling = 2;
		int mapWidth = s.nextInt();
		int mapHeight = s.nextInt();
		int screenWidth = s.nextInt();
		int screenHeight = s.nextInt();
		int vertOffset = ((2 * screenHeight - 1) * tileWidth * scaling) / 2;
		int horizOffset = screenWidth * tileWidth * scaling;
		Graphics2D g = mapImage.createGraphics();
		g.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 400));
		g.setColor(Color.BLACK);
		for (int j = 0; j < mapHeight; j++) {
			int yOffset = j * vertOffset;
			for (int i = 0; i < mapWidth; i++) {
				int xOffset = i * horizOffset;
				g.drawString(s.next(), xOffset + ((tileWidth * scaling) / 2), 
						yOffset + tileWidth * 9 * scaling);
			}
		}
		s.close();
		try {
			ImageIO.write(mapImage, "png", new File(outfilename));
		} catch (IOException e) {
			System.out.println("Error: failed to output image");
		}
	}
}
