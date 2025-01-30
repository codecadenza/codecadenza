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
package net.codecadenza.runtime.richclient.swing.widget;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;

/**
 * <p>
 * Implementation of a simple status panel
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JStatusPanel extends JPanel {
	private static final long serialVersionUID = -6662603863719739590L;

	private JPanel contentPanel;
	private JLabel messageLabel;
	private JProgressBar statusBar;
	private JLabel statusLabel;

	/**
	 * Constructor
	 */
	public JStatusPanel() {
		initiate();
	}

	/**
	 * Initialize the panel
	 */
	private void initiate() {
		initComponents();

		final int y = (int) statusBar.getPreferredSize().getHeight();

		statusBar.setMinimumSize(new Dimension(40, y));
		statusBar.setPreferredSize(new Dimension(60, y));

		setProgressMinimum(0);
		setProgressValue(0);
		setProgressMaximum(100);
		setProgressTextPainted(false);
	}

	/**
	 * Set the busy status of the status bar
	 * @param busy busy or not
	 */
	public void setBusy(boolean busy) {
		statusBar.setIndeterminate(busy);
	}

	/**
	 * Get the busy status of the status bar
	 * @return true if the component is busy
	 */
	public boolean isBusy() {
		return statusBar.isIndeterminate();
	}

	/**
	 * Set the message text
	 * @param text the text
	 */
	public void setMessage(String text) {
		messageLabel.setText(text);
	}

	/**
	 * Get the message text
	 * @return the message text
	 */
	public String getMessage() {
		return messageLabel.getText();
	}

	/**
	 * Clear the message text
	 */
	public void clearMessage() {
		messageLabel.setText("");
	}

	/**
	 * Set the status text
	 * @param text the text
	 */
	public void setStatus(String text) {
		statusLabel.setText(text);
	}

	/**
	 * Get the status text
	 * @return the status text
	 */
	public String getStatus() {
		return statusLabel.getText();
	}

	/**
	 * Clear the status text
	 */
	public void clearStatus() {
		statusLabel.setText("");
	}

	/**
	 * Set the message icon
	 * @param icon the icon
	 */
	public void setMessageIcon(ImageIcon icon) {
		messageLabel.setIcon(icon);
	}

	/**
	 * Set the status icon
	 * @param icon the icon
	 */
	public void setStatusIcon(ImageIcon icon) {
		statusLabel.setIcon(icon);
	}

	/**
	 * Clear the message icon
	 */
	public void clearMessageIcon() {
		messageLabel.setIcon(null);
	}

	/**
	 * Clear the status icon
	 */
	public void clearStatusIcon() {
		statusLabel.setIcon(null);
	}

	/**
	 * Set the status bar minimum
	 * @param minimum the value
	 */
	public void setProgressMinimum(int minimum) {
		statusBar.setMinimum(minimum);
	}

	/**
	 * Get the status bar minimum
	 * @return the minimum value
	 */
	public int getProgressMinimum() {
		return statusBar.getMinimum();
	}

	/**
	 * Set the status bar maximum
	 * @param maximum the value
	 */
	public void setProgressMaximum(int maximum) {
		statusBar.setMaximum(maximum);
	}

	/**
	 * Get the status bar maximum
	 * @return the maximum value
	 */
	public int getProgressMaximum() {
		return statusBar.getMaximum();
	}

	/**
	 * Set the status bar progress value
	 * @param value the value
	 */
	public void setProgressValue(int value) {
		statusBar.setValue(value);
	}

	/**
	 * Get the status bar progress value
	 * @return the value
	 */
	public int getProgressValue() {
		return statusBar.getValue();
	}

	/**
	 * Set the status bar progress text
	 * @param text the progress text
	 */
	public void setProgressText(String text) {
		statusBar.setString(text);
	}

	/**
	 * Get the status bar progress text
	 * @return the progress text
	 */
	public String getProgressText() {
		return statusBar.getString();
	}

	/**
	 * Set whether the status bar progress text is displayed
	 * @param state displayed or not
	 */
	public void setProgressTextPainted(boolean state) {
		statusBar.setStringPainted(state);
	}

	/**
	 * Get whether the status bar progress text is displayed
	 * @return true if the status bar progress text is displayed
	 */
	public boolean isProgressTextPainted() {
		return statusBar.isStringPainted();
	}

	/**
	 * Add a component to the status bar panel
	 * @param component the Swing component
	 */
	public void addComponent(JComponent component) {
		final var divide = new JSeparator(JSeparator.VERTICAL);
		divide.setPreferredSize(new Dimension(2, 14));

		contentPanel.add(divide);
		contentPanel.add(component);
	}

	/**
	 * Remove the component from the status bar
	 * @param component the swing component
	 */
	public void removeComponent(JComponent component) {
		final int divideIndex = contentPanel.getComponentZOrder(component) - 1;

		if (divideIndex >= 0) {
			contentPanel.remove(component);
			contentPanel.remove(divideIndex);
		}
	}

	/**
	 * Initialize all components
	 */
	private void initComponents() {
		final var separator = new JSeparator();
		messageLabel = new JLabel();
		statusLabel = new JLabel();
		statusBar = new JProgressBar();
		contentPanel = new JPanel();

		setLayout(new GridBagLayout());

		var gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridwidth = 4;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weightx = 1.0;

		add(separator, gridBagConstraints);

		messageLabel.setHorizontalAlignment(SwingConstants.LEFT);
		messageLabel.setHorizontalTextPosition(SwingConstants.LEFT);

		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridy = 1;
		gridBagConstraints.anchor = GridBagConstraints.WEST;
		gridBagConstraints.insets = new Insets(2, 6, 3, 0);

		add(messageLabel, gridBagConstraints);

		statusLabel.setHorizontalAlignment(SwingConstants.RIGHT);
		statusLabel.setHorizontalTextPosition(SwingConstants.LEFT);

		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 2;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.anchor = GridBagConstraints.WEST;
		gridBagConstraints.insets = new Insets(2, 6, 3, 0);

		add(statusLabel, gridBagConstraints);

		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 3;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.anchor = GridBagConstraints.EAST;
		gridBagConstraints.insets = new Insets(2, 12, 3, 5);

		add(statusBar, gridBagConstraints);

		contentPanel.setLayout(new FlowLayout(FlowLayout.LEFT));

		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.weightx = 1.0;

		add(contentPanel, gridBagConstraints);
	}

}
