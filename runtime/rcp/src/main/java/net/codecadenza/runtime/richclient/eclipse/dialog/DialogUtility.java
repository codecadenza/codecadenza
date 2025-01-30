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
package net.codecadenza.runtime.richclient.eclipse.dialog;

import java.awt.Dimension;
import java.awt.Toolkit;
import org.eclipse.swt.graphics.Point;

/**
 * <p>
 * Utility class to recalculate the size of a dialog if the target system has a DPI resolution other than 96
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DialogUtility {
	public static final int DEFAULT_DPI = 96;

	/**
	 * Prevent instantiation
	 */
	private DialogUtility() {

	}

	/**
	 * Recalculate the default dialog size if the target system has a DPI resolution other than 96
	 * @param defaultWidth
	 * @param defaultHeight
	 * @return the adapted size
	 */
	public static Point adaptSizeToSystemDPI(int defaultWidth, int defaultHeight) {
		// Get the system DPI
		final int systemDPI = Toolkit.getDefaultToolkit().getScreenResolution();

		if (systemDPI == DEFAULT_DPI)
			return new Point(defaultWidth, defaultHeight);

		// Get the size of the screen. On systems with multiple displays, the primary display is used.
		final Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();

		final Double factor = systemDPI / (double) DEFAULT_DPI;

		Double newWidth = defaultWidth * factor;
		Double newHeight = defaultHeight * factor;

		// The proposed size should not be greater than the current screen size!
		if (newWidth > dim.getWidth())
			newWidth = dim.getWidth();

		if (newHeight > dim.getHeight())
			newHeight = dim.getHeight();

		return new Point(newWidth.intValue(), newHeight.intValue());
	}

}
