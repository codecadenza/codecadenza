/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.tools.util.editor;

import java.util.HashMap;
import java.util.Map;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

/**
 * <p>
 * Colors used in the SQL editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ColorProvider {
	public static final RGB BACKGROUND = new RGB(255, 255, 255);
	public static final RGB MULTI_LINE_COMMENT = new RGB(64, 128, 128);
	public static final RGB SINGLE_LINE_COMMENT = new RGB(64, 128, 128);
	public static final RGB DEFAULT = new RGB(0, 0, 0);
	public static final RGB KEYWORD = new RGB(127, 0, 85);
	public static final RGB TABLE = new RGB(0, 0, 255);
	public static final RGB STRING = new RGB(153, 76, 0);
	public static final RGB ENTITY = new RGB(0, 0, 255);

	protected Map<RGB, Color> colorMap = HashMap.newHashMap(10);

	/**
	 * Dispose all colors
	 */
	public void dispose() {
		colorMap.values().stream().forEach(Color::dispose);
	}

	/**
	 * Get the requested color
	 * @param rgb
	 * @return the color
	 */
	public Color getColor(RGB rgb) {
		return colorMap.computeIfAbsent(rgb, key -> new Color(Display.getCurrent(), key));
	}

}
