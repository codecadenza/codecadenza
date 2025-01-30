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
package net.codecadenza.runtime.richclient.eclipse.search.util;

import net.codecadenza.runtime.search.SearchService;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * If a user performs a 'CTRL+V' on a text field the respective content in the clipboard will be converted by replacing all new
 * line characters (depending on the operating system) by
 * {@link net.codecadenza.runtime.search.SearchService#TOKEN_DELIMITER_IN}.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchInputPasteFromClipboardListener {
	private final Text textField;

	/**
	 * Constructor
	 * @param text
	 */
	public SearchInputPasteFromClipboardListener(Text text) {
		this.textField = text;

		// The keyPressed() method cannot be used as an appropriate event. When pressing 'CTRL+V' the event won't be propagated to the
		// listener!
		textField.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				if ((e.stateMask & SWT.CTRL) != 0 && ((char) e.keyCode == 'v')) {
					final var clipboard = new Clipboard(textField.getDisplay());
					final var content = (String) clipboard.getContents(TextTransfer.getInstance());
					final String convertedContent = content.replace(System.lineSeparator(), SearchService.TOKEN_DELIMITER_IN);

					textField.setText(convertedContent);

					clipboard.dispose();

					if (!content.equals(convertedContent))
						onContentConverted(convertedContent);
				}
			}
		});
	}

	/**
	 * Callback method that will be invoked as soon as the listener converts the given content from the clipboard. The method won't
	 * be invoked if the clipboard content was not converted!
	 * @param convertedContent
	 */
	@SuppressWarnings("unused")
	public void onContentConverted(String convertedContent) {
		// The implementation must be provided by a subclass!
	}

}
