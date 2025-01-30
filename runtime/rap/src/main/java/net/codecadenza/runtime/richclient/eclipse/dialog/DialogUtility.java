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

import org.eclipse.swt.graphics.Point;

/**
 * <p>
 * Utility class to recalculate the size of a dialog. Runtime tests with Eclipse RAP 3.0 have shown that the size of the dialogs
 * is too small when using default values for height and width defined in either internal runtime dialogs or proposed by
 * CodeCadenza's form generator. By using a simple correction factor the dialogs will be displayed in a reasonable manner.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DialogUtility {
	private static final double ENLARGE_FACTOR = 1.4;

	/**
	 * Prevent instantiation
	 */
	private DialogUtility() {

	}

	/**
	 * Recalculate the default dialog size
	 * @param defaultWidth
	 * @param defaultHeight
	 * @return the size
	 */
	public static Point adaptSizeToSystemDPI(int defaultWidth, int defaultHeight) {
		return new Point((int) (defaultWidth * ENLARGE_FACTOR), (int) (defaultHeight * ENLARGE_FACTOR));
	}

}
