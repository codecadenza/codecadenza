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
package net.codecadenza.runtime.richclient.swing.dialog;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.widget.JStatusPanel;
import net.codecadenza.runtime.richclient.swing.widget.StatusReceivable;

/**
 * <p>
 * Title area dialog with a status bar
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class JStatusTitleAreaDialog extends JTitleAreaDialog implements StatusReceivable {
	private static final long serialVersionUID = 1360141296978571855L;

	private JStatusPanel statusPanel;

	/**
	 * Constructor
	 */
	protected JStatusTitleAreaDialog() {
		this(null, true);
	}

	/**
	 * Constructor
	 * @param showTitle parameter that controls if title area should be added
	 */
	protected JStatusTitleAreaDialog(boolean showTitle) {
		this(null, showTitle);
	}

	/**
	 * Constructor
	 * @param parent
	 */
	protected JStatusTitleAreaDialog(Component parent) {
		this(parent, true);
	}

	/**
	 * Constructor
	 * @param parent
	 * @param showTitle
	 */
	protected JStatusTitleAreaDialog(Component parent, boolean showTitle) {
		super(parent, showTitle);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setBusy(boolean)
	 */
	@Override
	public void setBusy(boolean busy) {
		statusPanel.setBusy(busy);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setStatusIcon(javax.swing.ImageIcon)
	 */
	@Override
	public void setStatusIcon(ImageIcon statusIcon) {
		statusPanel.setStatusIcon(statusIcon);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setStatusErrorMessage(java.lang.String)
	 */
	@Override
	public void setStatusErrorMessage(String message) {
		statusPanel.setMessage(message);
		statusPanel.setMessageIcon(ImageLoader.getImage(ImageLoader.ERROR));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setStatusInfoMessage(java.lang.String)
	 */
	@Override
	public void setStatusInfoMessage(String message) {
		statusPanel.setMessage(message);
		statusPanel.setMessageIcon(ImageLoader.getImage(ImageLoader.INFO));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#buildBaseContent()
	 */
	@Override
	protected void buildBaseContent() {
		getContentPane().setLayout(new BorderLayout());

		if (showTitle) {
			panTitle = new TitleAreaPanel("");
			getContentPane().add(panTitle, BorderLayout.NORTH);
		}

		panDialog.setBorder(new EmptyBorder(5, 5, 5, 5));

		getContentPane().add(panDialog, BorderLayout.CENTER);

		final var panBottom = new JPanel();

		final var gblBottom = new GridBagLayout();
		gblBottom.columnWeights = new double[] { 1.0, Double.MIN_VALUE };
		gblBottom.rowWeights = new double[] { 1.0, 0 };

		panBottom.setLayout(gblBottom);

		getContentPane().add(panBottom, BorderLayout.SOUTH);

		final var gbcButton = new GridBagConstraints();
		gbcButton.insets = new Insets(0, 0, 0, 0);
		gbcButton.fill = GridBagConstraints.HORIZONTAL;
		gbcButton.gridx = 0;
		gbcButton.gridy = 0;

		panButton = new JPanel();
		panButton.setLayout(new FlowLayout(FlowLayout.RIGHT));

		panBottom.add(panButton, gbcButton);

		final var gbcStatus = new GridBagConstraints();
		gbcStatus.insets = new Insets(0, 0, 0, 0);
		gbcStatus.fill = GridBagConstraints.HORIZONTAL;
		gbcStatus.gridx = 0;
		gbcStatus.gridy = 1;

		statusPanel = new JStatusPanel();

		panBottom.add(statusPanel, gbcStatus);
	}

}
