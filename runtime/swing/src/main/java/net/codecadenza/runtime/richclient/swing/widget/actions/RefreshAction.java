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
package net.codecadenza.runtime.richclient.swing.widget.actions;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.REFRESH_ACTION_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.REFRESH_ACTION_SHORT_DESC;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel;

/**
 * <p>
 * Action to perform a refresh
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RefreshAction extends AbstractAction {
	private static final long serialVersionUID = -7367711277496630119L;

	private final AbstractDataTablePanel<?> dataPanel;

	/**
	 * Constructor
	 * @param dataPanel
	 */
	public RefreshAction(AbstractDataTablePanel<?> dataPanel) {
		super(getTranslation(REFRESH_ACTION_NAME), ImageLoader.getImage(ImageLoader.REFRESH));

		putValue(SHORT_DESCRIPTION, getTranslation(REFRESH_ACTION_SHORT_DESC));
		this.dataPanel = dataPanel;
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		dataPanel.performFetch();
	}

}
