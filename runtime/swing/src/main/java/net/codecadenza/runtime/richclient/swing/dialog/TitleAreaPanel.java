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

import java.awt.Color;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;

/**
 * <p>
 * Panel for the title area
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TitleAreaPanel extends JPanel {
	private static final long serialVersionUID = -8328256680047594443L;

	private final JLabel lblTitle;
	private final JLabel lblTitleImage;
	private final JLabel lblMessage;

	/**
	 * @param message
	 */
	public void setTitleMessage(String message) {
		lblTitle.setText(message);
	}

	/**
	 * Set the title image
	 * @param image
	 */
	public void setTitleImage(Icon image) {
		lblTitleImage.setIcon(image);
	}

	/**
	 * Set the error message
	 * @param message
	 */
	public void setErrorMessage(String message) {
		lblMessage.setIcon(ImageLoader.getImage(ImageLoader.ERROR));
		lblMessage.setText(message);
	}

	/**
	 * Set the info message
	 * @param message
	 */
	public void setInformationMessage(String message) {
		lblMessage.setIcon(null);
		lblMessage.setText(message);
	}

	/**
	 * Constructor
	 * @param title
	 * @param message
	 * @param titleImage
	 */
	public TitleAreaPanel(String title, String message, Icon titleImage) {
		this(title);

		lblMessage.setText(message);
		lblTitleImage.setIcon(titleImage);
	}

	/**
	 * Constructor
	 * @param title
	 * @param message
	 */
	public TitleAreaPanel(String title, String message) {
		this(title);

		lblMessage.setText(message);
	}

	/**
	 * Constructor
	 * @param title
	 */
	public TitleAreaPanel(String title) {
		setForeground(Color.LIGHT_GRAY);
		setBorder(new CompoundBorder(new LineBorder(new Color(192, 192, 192)), new EmptyBorder(5, 5, 5, 5)));
		setBackground(Color.WHITE);

		final var gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[] { 0, 0, 0, 0 };
		gridBagLayout.rowHeights = new int[] { 0, 0, 0 };
		gridBagLayout.columnWeights = new double[] { 0.0, 1.0, 0.0, Double.MIN_VALUE };
		gridBagLayout.rowWeights = new double[] { 0.0, 0.0, Double.MIN_VALUE };

		setLayout(gridBagLayout);

		lblTitle = new JLabel();

		final Font f = lblTitle.getFont();
		final var newTitleFont = new Font(f.getName(), Font.BOLD, f.getSize() + 1);

		lblTitle.setFont(newTitleFont);

		final var gbcLblTitle = new GridBagConstraints();
		gbcLblTitle.anchor = GridBagConstraints.WEST;
		gbcLblTitle.insets = new Insets(0, 0, 5, 5);
		gbcLblTitle.gridx = 0;
		gbcLblTitle.gridy = 0;

		add(lblTitle, gbcLblTitle);

		lblTitleImage = new JLabel(title);
		lblTitleImage.setIcon(ImageLoader.getImage(ImageLoader.DEFAULT_TITLE_IMAGE));

		final var gbcTitleImage = new GridBagConstraints();
		gbcTitleImage.anchor = GridBagConstraints.EAST;
		gbcTitleImage.gridheight = 2;
		gbcTitleImage.gridx = 2;
		gbcTitleImage.gridy = 0;

		add(lblTitleImage, gbcTitleImage);

		lblMessage = new JLabel();

		final var gbcMessage = new GridBagConstraints();
		gbcMessage.gridwidth = 2;
		gbcMessage.anchor = GridBagConstraints.WEST;
		gbcMessage.insets = new Insets(0, 0, 0, 5);
		gbcMessage.gridx = 0;
		gbcMessage.gridy = 1;

		add(lblMessage, gbcMessage);
	}

}
