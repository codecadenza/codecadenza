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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.J_TITLE_AREA_DIALOG_CANCEL_BUTTON;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.J_TITLE_AREA_DIALOG_OK_BUTTON;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.KeyStroke;
import javax.swing.border.EmptyBorder;

/**
 * <p>
 * Title area dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class JTitleAreaDialog extends JDialog {
	private static final long serialVersionUID = -4792118071006227368L;
	public static final int RETURN_CODE_OK = 1;
	public static final int RETURN_CODE_CANCEL = 2;

	protected final JPanel panDialog = new JPanel();
	protected TitleAreaPanel panTitle;
	private int returnCode = RETURN_CODE_CANCEL;
	protected boolean showTitle;
	private boolean initialized;
	private String titleMessage = "";
	protected JPanel panButton;
	protected JButton okButton;

	/**
	 * Constructor
	 */
	protected JTitleAreaDialog() {
		this(null, true);
	}

	/**
	 * Constructor
	 * @param showTitle parameter that controls if title area should be added
	 */
	protected JTitleAreaDialog(boolean showTitle) {
		this(null, showTitle);
	}

	/**
	 * Constructor
	 * @param parent
	 */
	protected JTitleAreaDialog(Component parent) {
		this(parent, true);
	}

	/**
	 * Constructor
	 * @param parent
	 * @param showTitle
	 */
	protected JTitleAreaDialog(Component parent, boolean showTitle) {
		super(JOptionPane.getFrameForComponent(parent));

		this.showTitle = showTitle;
		buildBaseContent();
	}

	/**
	 * @return the return code
	 */
	public int getReturnCode() {
		return returnCode;
	}

	/**
	 * @param returnCode
	 */
	public void setReturnCode(int returnCode) {
		this.returnCode = returnCode;
	}

	/**
	 * Set the title message
	 * @param message
	 */
	public void setTitleMessage(String message) {
		if (showTitle)
			panTitle.setTitleMessage(message);
		else
			titleMessage = message;
	}

	/**
	 * Set the error message
	 * @param message
	 */
	public void setErrorMessage(String message) {
		if (showTitle)
			panTitle.setErrorMessage(message);
		else
			JOptionPane.showMessageDialog(this, message, titleMessage, JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * Set the info message
	 * @param message
	 */
	public void setInformationMessage(String message) {
		if (showTitle)
			panTitle.setInformationMessage(message);
		else
			JOptionPane.showMessageDialog(this, message, titleMessage, JOptionPane.INFORMATION_MESSAGE);
	}

	/**
	 * Set the title image
	 * @param image
	 */
	public void setTitleImage(Icon image) {
		if (showTitle)
			panTitle.setTitleImage(image);
	}

	/**
	 * Create the contents of the dialog
	 * @param contentPane
	 */
	public abstract void createContents(JPanel contentPane);

	/**
	 * Callback handler if the user clicked the 'OK' button
	 */
	public void onOKClicked() {

	}

	/**
	 * Callback handler if the user clicked the 'Cancel' button
	 */
	public void onCancelClicked() {
		dispose();
	}

	/*
	 * (non-Javadoc)
	 * @see javax.swing.JDialog#createRootPane()
	 */
	@Override
	protected JRootPane createRootPane() {
		final ActionListener actionListener = _ -> dispose();
		final var rootPane = new JRootPane();
		final KeyStroke stroke = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);

		rootPane.registerKeyboardAction(actionListener, stroke, JComponent.WHEN_IN_FOCUSED_WINDOW);
		return rootPane;
	}

	/**
	 * Build the basic content of the dialog
	 */
	protected void buildBaseContent() {
		getContentPane().setLayout(new BorderLayout());

		if (showTitle) {
			panTitle = new TitleAreaPanel("");
			getContentPane().add(panTitle, BorderLayout.NORTH);
		}

		panDialog.setBorder(new EmptyBorder(5, 5, 5, 5));

		getContentPane().add(panDialog, BorderLayout.CENTER);

		panButton = new JPanel();
		panButton.setLayout(new FlowLayout(FlowLayout.RIGHT));

		getContentPane().add(panButton, BorderLayout.SOUTH);
	}

	/*
	 * (non-Javadoc)
	 * @see java.awt.Dialog#setVisible(boolean)
	 */
	@Override
	public void setVisible(boolean b) {
		if (!initialized && b) {
			initialized = true;
			createContents(panDialog);
			createButtons(panButton);
		}

		super.setVisible(b);
	}

	/**
	 * Create the dialog buttons. By default, an 'OK' and a 'Cancel' button are created. This method may be overridden by
	 * subclasses!
	 * @param buttonPane the button panel
	 */
	protected void createButtons(JPanel buttonPane) {
		okButton = new JButton(getTranslation(J_TITLE_AREA_DIALOG_OK_BUTTON));
		okButton.addActionListener(_ -> onOKClicked());
		okButton.setActionCommand(getTranslation(J_TITLE_AREA_DIALOG_OK_BUTTON));

		buttonPane.add(okButton);

		getRootPane().setDefaultButton(okButton);

		final var cancelButton = new JButton(getTranslation(J_TITLE_AREA_DIALOG_CANCEL_BUTTON));
		cancelButton.addActionListener(_ -> onCancelClicked());
		cancelButton.setActionCommand(getTranslation(J_TITLE_AREA_DIALOG_CANCEL_BUTTON));

		buttonPane.add(cancelButton);
	}

	/**
	 * @return the panel where the content of the dialog should be added to
	 */
	public JPanel getDialogPanel() {
		return panDialog;
	}

}
