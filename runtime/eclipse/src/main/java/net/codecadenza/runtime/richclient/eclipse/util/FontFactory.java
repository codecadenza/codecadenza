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
package net.codecadenza.runtime.richclient.eclipse.util;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;

/**
 * <p>
 * Font factory
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FontFactory {
	private static Font labelFont;

	/**
	 * Prevent instantiation
	 */
	private FontFactory() {

	}

	/**
	 * @return the standard font for labels
	 */
	public static synchronized Font getLabelFont() {
		if (labelFont == null) {
			final Font defaultFont = Display.getDefault().getSystemFont();

			final var fontData = new FontData();
			fontData.setName(defaultFont.getFontData()[0].getName());
			fontData.setHeight(defaultFont.getFontData()[0].getHeight());
			fontData.setStyle(SWT.BOLD);

			labelFont = new Font(Display.getDefault(), fontData);
		}

		return labelFont;
	}

}
