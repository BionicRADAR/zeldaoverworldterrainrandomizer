/**
 * This program reads in data on a legend of zelda map and a png 
 * image file containing the various game tiles and outputs an image of
 * the complete map in png format.
 * It requires the file zeldaoverworldtiles.png to be in the same directory
 * as it to run properly.
 * The filename of the file containing the map data should be passed in as 
 * an arg to the program. An example input file is included in "example.txt",
 * with its output in "example.png".
 * A description of the form of the input file is provided in "description.txt".
 *
 * Author: Nathaniel Schleicher
 * Written 11 March 2016
 */
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Scanner;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;
import java.awt.Graphics2D;
import java.awt.Color;
import java.awt.BasicStroke;

public class ZeldaDisplay {
	private static BufferedImage tiles, output;
	private static IntPair[] brown, green, gray;
	private static int tileWidth = 16;
	private static int tileOffset = 17;
	private static int scaling = 2;
	private static class IntPair {
		private int x, y;
		public IntPair(int x, int y) {
			this.x = x;
			this.y = y;
		}
		public int x() {return x;}
		public int y() {return y;}
	}

	private static void initTileArrays() {
		brown = new IntPair[64];
		green = new IntPair[64];
		gray = new IntPair[64];
		//middle water #x05
		addTrio(5, 1, 5);
		//north water #x06
		addTrio(6, 1, 4);
		//south water #x07
		addTrio(7, 1, 6);
		//west water #x08
		addTrio(8, 0, 5);
		//east water #x09
		addTrio(9, 2, 5);
		//ladder #x0a
		addTrio(10, 0, 1);
		//dock #x0b
		addTrio(11, 5, 7);
		//cave #x0c
		addSingle(12, 4, 1);
		//empty #x0e
		brown[14] = new IntPair(2, 0);
		green[14] = new IntPair(2, 0);
		gray[14] = new IntPair(14, 0);
		//waterfall-exit #x0f
		brown[15] = new IntPair(4, 7);
		green[15] = new IntPair(18, 3);
		gray[15] = new IntPair(18, 5);
		//stairwell #x12
		addTrio(18, 0, 0);
		//boulder #x13
		addTrio(19, 1, 0);
		//grave #x14; 
		brown[20] = new IntPair(18, 0);
		green[20] = new IntPair(18, 1);
		gray[20] = new IntPair(13, 1);
		//northwest water #x15
		addTrio(21, 0, 4);
		//northeast water #x16
		addTrio(22, 2, 4);
		//southwest water #x17
		addTrio(23, 0, 6);
		//southeast water #x18
		addTrio(24, 2, 6);
		//shrub #x19
		brown[25] = new IntPair(1, 1);
		green[25] = new IntPair(7, 1);
		gray[25] = new IntPair(8, 0);
		//bottom cliff wall #x1a
		addTrio(26, 1, 2);
		//full cliff wall #x1b
		addTrio(27, 1, 3);
		//top-left tree #x1c
		addTrio(28, 3, 0);
		//top-middle tree #x1d
		addTrio(29, 4, 0);
		//top-right tree #x1e
		addTrio(30, 5, 0);
		//bottom-left tree #x1f
		addTrio(31, 3, 1);
		//bottom-right tree #x20
		addTrio(32, 5, 1);
		//top-left ruin #x21
		addTrio(33, 3, 2);
		//two-eye ruin #x22
		brown[34] = new IntPair(4, 2);
		green[34] = new IntPair(10, 2);
		gray[34] = new IntPair(10, 7);
		//top-right ruin #x23
		addTrio(35, 5, 2);
		//bottom-left ruin #x24
		addTrio(36, 3, 3);
		//bottom-right ruin #x25
		addTrio(37, 5, 3);
		//moveable boulder #x26 (same as unmoveable boulder)
		addTrio(38, 1, 0);
		//bombable wall #x27 (same as cliff wall)
		addTrio(39, 1, 3);
		//burnable shrub #x28 (same as normal shrub)
		brown[40] = new IntPair(1, 1);
		green[40] = new IntPair(7, 1);
		gray[40] = new IntPair(8, 0);
		//pushable grave #x29 (same as normal grave)
		brown[41] = new IntPair(18, 0);
		green[41] = new IntPair(18, 1);
		gray[41] = new IntPair(13, 1);
		//armos1 #x2a
		addTrio(42, 2, 1);
		//armos2 #x2b (same as armos1)
		addTrio(43, 2, 1);
		//armos3 #x2c (same as armos2 and armos1)
		addTrio(44, 2, 1);
		//one-eye ruin #x2d;
		brown[45] = new IntPair(16, 7);
		green[45] = new IntPair(10, 1);
		gray[45] = new IntPair(16, 2);
		//northwest shore #x2e
		addTrio(46, 0, 7);
		//northeast shore #x2f
		addTrio(47, 1, 7);
		//southwest shore #x30
		addTrio(48, 2, 7);
		//southeast shore #x31
		addTrio(49, 3, 7);
		//southeast cliff wall #x32
		addTrio(50, 0, 2);
		//northeast cliff wall #x33
		addTrio(51, 2, 2);
		//southwest cliff wall #x34
		addTrio(52, 0, 3);
		//southeast cliff wall #x35
		addTrio(53, 2, 3);
		//waterfall #x36
		brown[54] = new IntPair(10, 3);
		green[54] = new IntPair(18, 4);
		gray[54] = new IntPair(18, 6);
		//desert #x37
		brown[55] = new IntPair(4, 3);
		green[55] = new IntPair(18, 2);
		gray[55] = new IntPair(16, 3);
	}

	private static boolean isMutable(int tile) {
		return (tile > 37 && tile < 42);
	}

	private static IntPair getTileOffset(IntPair tileLoc) {
		return new IntPair(1 + tileOffset * tileLoc.x(),
						   1 + tileOffset * tileLoc.y());
	}

	private static void addTrio(int index, int baseX, int y) {
		brown[index] = new IntPair(baseX, y);
		green[index] = new IntPair(baseX + 6, y);
		gray[index] = new IntPair(baseX + 12, y);
	}

	private static void addSingle(int index, int x, int y) {
		brown[index] = new IntPair(x, y);
		green[index] = new IntPair(x, y);
		gray[index] = new IntPair(x, y);
	}

	private static void drawTile(Graphics2D g, int tile, IntPair[] palette,
								 IntPair mapOffset, int x, int y, 
								 boolean outline) {
		int dLeft = mapOffset.x() + x * tileWidth * scaling;
		int dTop = mapOffset.y() + y * tileWidth * scaling;
		int dWidth = tileWidth * scaling;
		int sLeft = 1 + palette[tile].x() * tileOffset;
		int sTop = 1 + palette[tile].y() * tileOffset;
		g.drawImage(tiles, dLeft, dTop, dLeft + dWidth, dTop + dWidth,
				    sLeft, sTop, sLeft + tileWidth, sTop + tileWidth, null);
		if (isMutable(tile) || outline) {
			g.setColor(Color.red);
			g.setStroke(new BasicStroke(scaling));
			g.drawRect(dLeft - 1, dTop - 1, dWidth, dWidth);
		}
	}

	private static void drawScreen(Graphics2D g, int x, int y, int sWidth, 
									int sHeight, Scanner input) {
		IntPair[] outer, inner;
		outer = getPalette(input.next());
		inner = getPalette(input.next());
		String special = input.next();
		int specX = -1;
		int specY = -1;
		try {
			specX = Integer.parseInt(special);
			specY = Integer.parseInt(input.next());
		} catch (NumberFormatException e) {}
		IntPair mapOffset = new IntPair(x, y);
		for (int j = 0; j < sHeight; j++) 
			for (int i = 0; i < sWidth; i++) {
				IntPair[] palette = inner;
				if (j < 2 || j > sHeight - 3 || i < 2 || i > sWidth - 3)
					palette = outer;
				boolean outline = false;
				if (i == specX && j == specY)
					outline = true;
				drawTile(g, getNextInt(input), palette, mapOffset, 
							i, j, outline);
			}
		if (special.toLowerCase().equals("recorder"))
			drawTile(g, 18, inner, mapOffset, 6, 5, false);
	}
	
	public static void main(String[] args) {
		initTileArrays();
		if (args.length < 1) {
			System.out.println("Error: no filename provided");
			return;
		}
		String filename = "map.png";
		if (args.length > 1) {
			filename = args[1];
			if (!filename.substring(filename.length() - 4).equals(".png"))
				filename = filename + ".png";
		}
		try {
			tiles = ImageIO.read(new File("zeldaoverworldtiles.png"));
		} catch (IOException e) {
			System.out.println("Error: failed to load zeldaoverworldtiles.png");
			return;
		}
		if (args.length > 2) {
			if (args[2].equals("-c"))
				displayColumns(args[0], filename);
			if (args[2].equals("-bc"))
				displayByteColumns(args[0], filename);
			return;
		}	
		Scanner s = null;
		try {
			s = new Scanner(new File(args[0]));
		} catch (FileNotFoundException e) {
			System.out.println("Error: input file not found");
			return;
		}
		int mapWidth = s.nextInt();
		int mapHeight = s.nextInt();
		int screenWidth = s.nextInt();
		int screenHeight = s.nextInt();	
		int vertOffset = ((2 * screenHeight - 1) * tileWidth * scaling) / 2;
		output = new BufferedImage(mapWidth * screenWidth * tileWidth * scaling,
								mapHeight * vertOffset, 
								BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = output.createGraphics();
		for (int j = 0; j < mapHeight; j++) {
			int yOffset = j * vertOffset;
			for (int i = 0; i < mapWidth; i++){
				int xOffset = i * screenWidth * tileWidth * scaling;
				drawScreen(g, xOffset, yOffset, screenWidth, screenHeight, s);
				if (j == mapHeight - 1 && i > 0) {
					g.setColor(Color.black);
					g.setStroke(new BasicStroke(scaling));
					g.drawLine(xOffset, 0, xOffset, output.getHeight());
				}
			}
			if (j > 0) {
				g.setColor(Color.black);
				g.setStroke(new BasicStroke(scaling));
				g.drawLine(0, yOffset, output.getWidth(), yOffset);
			}
		}
		g.dispose();
		try {
			ImageIO.write(output, "png", new File(filename));
		} catch (IOException e) {
			System.out.println("Error: failed to output image");
			return;
		}
	}

	private static int getNextInt(Scanner s) {
		return Integer.parseInt(s.next(), 16);
	}

	private static IntPair[] getPalette(String paletteName) {
		if (paletteName.toLowerCase().equals("green"))
			return green;
		if (paletteName.toLowerCase().equals("brown"))
			return brown;
		if (paletteName.toLowerCase().equals("gray"))
			return gray;
		System.out.println("Unrecognized palette: " + paletteName);
		return brown;
	}

	private static void displayByteColumns(String infile, String outfile) {
		Scanner s = null;
		try {
			s = new Scanner(new File(infile));
		} catch (FileNotFoundException e) {
			System.out.println("Error: input file not found");
			return;
		}
		int numColumns = s.nextInt();
		int numTiles = s.nextInt();
		String[] columnNums = new String[numColumns];
		int[] tileNums = new int[numTiles];
		boolean[] isOverlap = new boolean[numTiles];
		for (int i = 0; i < numColumns; i++) {
			columnNums[i] = s.next();
		}
		for (int i = 0; i < tileNums.length; i++) {
			tileNums[i] = getNextInt(s);
			isOverlap[i] = false;
		}
		output = new BufferedImage((tileOffset + 2) * 38 * scaling,
								4 * 12 * tileWidth * scaling,
								BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = output.createGraphics();
		g.setColor(Color.white);
		g.fillRect(0, 0, output.getWidth(), output.getHeight());
		IntPair[] palette = getPalette("brown");
		int tileBase = 0;
		for (int column = 0; column < numColumns; column++) {
			int vertBase = (column / 38) * 12 * tileWidth * scaling;
			System.out.println(vertBase);
			g.setColor(Color.black);
			g.drawString(columnNums[column], (column % 38) * (tileOffset + 2) * scaling, vertBase + tileWidth * scaling);
			int[] colTiles = new int[11];
			boolean[] colOverlap = new boolean[11];
			boolean[] colDouble = new boolean[11];
			int colBreak = -1;
			for (int j = tileBase + 1; j < numTiles; j++) {
				if (tileNums[j] >= 0x80) {
					colBreak = j;
					break;
				}
			}
			int tilePos = tileBase;
			tileBase = colBreak;
			for (int j = 0; j < 11; tilePos++) {
				if (tilePos >= colBreak && colBreak > -1) {
					isOverlap[tilePos] = true;
				}
				int tileTemp = tileNums[tilePos] % 0x40;
				if (isOverlap[tilePos] == true) {
					colOverlap[j] = true;
				} else {
					colOverlap[j] = false;
				}
				colTiles[j++] = tileTemp;
				if (tileNums[tilePos] % 0x80 >= 0x40 && j < 11) {
					if (isOverlap[tilePos] == true) {
						colOverlap[j] = true;
					} else {
						colOverlap[j] = false;
					}
					colDouble[j - 1] = true;
					colDouble[j] = true;
					colTiles[j++] = tileTemp;
				}
				else {
					colDouble[j - 1] = false;
				}
			}
			for (int j = 1; j < 12; j++) {
				int tile = colTiles[j - 1];
				boolean overlap = colOverlap[j - 1];
				boolean isDouble = colDouble[j - 1];
				int dWidth = scaling * tileWidth;
				int dLeft = (column % 38) * (tileOffset + 2) * scaling;
				int dTop = j * dWidth + vertBase;
				if (palette[tile] == null)
					continue;
				int sLeft = 1 + palette[tile].x() * tileOffset;
				int sTop = 1 + palette[tile].y() * tileOffset;
				g.drawImage(tiles, dLeft + scaling, dTop, 
						dLeft + scaling + dWidth, dTop + dWidth,
				    sLeft, sTop, sLeft + tileWidth, sTop + tileWidth, null);
				if (overlap) {
					g.setColor(new Color(0, 150, 0));
				}
				else {
					g.setColor(Color.white);
				}
				g.fillRect(dLeft, dTop, scaling, dWidth);
				if (isDouble) {
					g.setColor(new Color(150, 0, 255));
				}
				else {
					g.setColor(Color.white);
				}
				g.fillRect(dLeft + scaling + dWidth, dTop, scaling, dWidth);
				if (isMutable(tile)) {
					g.setColor(Color.red);
					g.setStroke(new BasicStroke(scaling));
					g.drawRect(dLeft + scaling - 1, dTop - 1, dWidth, dWidth);
				}
			}
		}
		try {
			ImageIO.write(output, "png", new File(outfile));
		} catch (IOException e) {
			System.out.println("Error: failed to output image");
			return;
		}
	}

	private static void displayColumns(String infile, String outfile) {
		Scanner s = null;
		try {
			s = new Scanner(new File(infile));
		} catch (FileNotFoundException e) {
			System.out.println("Error: input file not found");
			return;
		}
		int numColumns = s.nextInt();
		output = new BufferedImage(tileOffset * 38 * scaling,
								4 * 12 * tileWidth * scaling,
								BufferedImage.TYPE_INT_ARGB);
		System.out.println(output.getHeight());
		Graphics2D g = output.createGraphics();
		IntPair[] palette = getPalette("brown");
		//g.setColor(Color.white);
		//g.fillRect(0, 0, tileOffset * numColumns * scaling, tileWidth * scaling);
		for (int i = 0; i < numColumns; i++) {
			int vertBase = (i / 38) * 12 * tileWidth * scaling;
			System.out.println(vertBase);
			g.setColor(Color.black);
			g.drawString("" + getNextInt(s), (i % 38) * tileOffset * scaling, vertBase + tileWidth * scaling);
			for (int j = 1; j < 12; j++) {
				int tile = getNextInt(s);
				int dWidth = scaling * tileWidth;
				int dLeft = (i % 38) * tileOffset * scaling;
				int dTop = j * dWidth + vertBase;
				if (palette[tile] == null)
					continue;
				int sLeft = 1 + palette[tile].x() * tileOffset;
				int sTop = 1 + palette[tile].y() * tileOffset;
				g.drawImage(tiles, dLeft, dTop, dLeft + dWidth, dTop + dWidth,
				    sLeft, sTop, sLeft + tileWidth, sTop + tileWidth, null);
				if (isMutable(tile)) {
					g.setColor(Color.red);
					g.setStroke(new BasicStroke(scaling));
					g.drawRect(dLeft - 1, dTop - 1, dWidth, dWidth);
				}
			}
		}
		try {
			ImageIO.write(output, "png", new File(outfile));
		} catch (IOException e) {
			System.out.println("Error: failed to output image");
			return;
		}
	}
}
