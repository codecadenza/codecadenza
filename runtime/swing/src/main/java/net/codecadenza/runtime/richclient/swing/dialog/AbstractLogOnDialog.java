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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_INFO_MESSAGE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_LBL_HOST;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_LBL_PASSWORD;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_LBL_USER_NAME;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_MSG_CONNECT_TO_HOST;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_MSG_INVALID_CREDENTIALS;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_HOST;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_USER;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOG_ON_DIALOG_TITLE_MESSAGE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslationForFieldLabel;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.lang.invoke.MethodHandles;
import java.util.Map;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import net.codecadenza.runtime.richclient.logon.LogOnManager;
import net.codecadenza.runtime.richclient.persistence.entity.LastLogOn;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.widget.JDataComboBox;
import net.codecadenza.runtime.richclient.transport.ServiceLocatorDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for log-on dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractLogOnDialog extends JTitleAreaDialog {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -4092894102818674370L;
	private static final int MAX_WRONG_INPUTS = 3;

	private JTextField txtUserName;
	private JPasswordField txtPassword;
	private int wrongInputCount;
	private String userName = "";
	private String password = "";
	private transient ServiceLocatorDTO serviceLocatorDTO;
	private final transient Map<String, ServiceLocatorDTO> hostMap;
	private JDataComboBox<ServiceLocatorDTO> cboHost;
	private boolean hideHostSelection;

	/**
	 * Constructor
	 * @param hostMap a map that contains all available connection configurations
	 */
	protected AbstractLogOnDialog(Map<String, ServiceLocatorDTO> hostMap) {
		this.hostMap = hostMap;

		// If the caller just gives us a list containing one element we will hide the respective selection component!
		if (hostMap.size() == 1) {
			hideHostSelection = true;

			hostMap.values().stream().findFirst().ifPresent(dto -> serviceLocatorDTO = dto);
		}
	}

	/**
	 * @param settings
	 * @throws SecurityException if the login has failed
	 */
	public abstract void logOn(ServiceLocatorDTO settings);

	/**
	 * @return the selected user name
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * @return the selected service locator DTO
	 */
	public ServiceLocatorDTO getServiceLocatorDTO() {
		return serviceLocatorDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#createContents(javax.swing.JPanel)
	 */
	@Override
	public void createContents(JPanel contentPane) {
		final LastLogOn logOnInfo = LogOnManager.getLastLogOn();

		setReturnCode(RETURN_CODE_CANCEL);
		setSize(320, 210);
		setResizable(false);
		setModal(true);

		// Center the dialog on the screen
		setLocationRelativeTo(null);

		setTitle(getTranslation(ABSTRACT_LOG_ON_DIALOG_TITLE));
		setTitleMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_TITLE_MESSAGE));
		setInformationMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_INFO_MESSAGE));
		setTitleImage(ImageLoader.getImage(ImageLoader.LOGON));

		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));

		final var gblContentPane = new GridBagLayout();
		gblContentPane.columnWidths = new int[] { 0, 250, 0 };
		gblContentPane.rowHeights = new int[] { 0, 0, 0, 0 };
		gblContentPane.columnWeights = new double[] { 0.0, 1.0, Double.MIN_VALUE };

		if (!hideHostSelection)
			gblContentPane.rowWeights = new double[] { 0.0, 0.0, 0.0, Double.MIN_VALUE };
		else
			gblContentPane.rowWeights = new double[] { 0.0, 0.0, Double.MIN_VALUE };

		contentPane.setLayout(gblContentPane);

		final var lblUserName = new JLabel(getTranslationForFieldLabel(ABSTRACT_LOG_ON_DIALOG_LBL_USER_NAME));

		final var gbcLabelUserName = new GridBagConstraints();
		gbcLabelUserName.insets = new Insets(0, 0, 5, 5);
		gbcLabelUserName.anchor = GridBagConstraints.WEST;
		gbcLabelUserName.gridx = 0;
		gbcLabelUserName.gridy = 0;

		contentPane.add(lblUserName, gbcLabelUserName);

		txtUserName = new JTextField();

		final var gbcTextUserName = new GridBagConstraints();
		gbcTextUserName.insets = new Insets(0, 0, 5, 0);
		gbcTextUserName.fill = GridBagConstraints.HORIZONTAL;
		gbcTextUserName.gridx = 1;
		gbcTextUserName.gridy = 0;

		contentPane.add(txtUserName, gbcTextUserName);

		final var lblPassword = new JLabel(getTranslationForFieldLabel(ABSTRACT_LOG_ON_DIALOG_LBL_PASSWORD));

		final var gbcLabelPassword = new GridBagConstraints();
		gbcLabelPassword.anchor = GridBagConstraints.WEST;
		gbcLabelPassword.insets = new Insets(0, 0, 0, 5);
		gbcLabelPassword.gridx = 0;
		gbcLabelPassword.gridy = 1;

		contentPane.add(lblPassword, gbcLabelPassword);

		txtPassword = new JPasswordField();

		final var gbcTextPassword = new GridBagConstraints();
		gbcTextPassword.fill = GridBagConstraints.HORIZONTAL;
		gbcTextPassword.gridx = 1;
		gbcTextPassword.gridy = 1;

		contentPane.add(txtPassword, gbcTextPassword);

		if (!hideHostSelection) {
			final var lblHost = new JLabel(getTranslationForFieldLabel(ABSTRACT_LOG_ON_DIALOG_LBL_HOST));

			final var gbcLabelHost = new GridBagConstraints();
			gbcLabelHost.anchor = GridBagConstraints.WEST;
			gbcLabelHost.insets = new Insets(5, 0, 0, 5);
			gbcLabelHost.gridx = 0;
			gbcLabelHost.gridy = 2;

			contentPane.add(lblHost, gbcLabelHost);

			cboHost = new JDataComboBox<>(hostMap.values().stream().toList()) {
				private static final long serialVersionUID = 6647437968737418666L;

				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.swing.widget.JDataComboBox#getItemText(java.lang.Object)
				 */
				@Override
				public String getItemText(ServiceLocatorDTO element) {
					return element.getAlias();
				}
			};

			final var gbcTextHost = new GridBagConstraints();
			gbcTextHost.insets = new Insets(5, 0, 0, 0);
			gbcTextHost.fill = GridBagConstraints.HORIZONTAL;
			gbcTextHost.gridx = 1;
			gbcTextHost.gridy = 2;

			contentPane.add(cboHost, gbcTextHost);
		}

		if (logOnInfo != null) {
			txtUserName.setText(logOnInfo.getUserName());

			// Workaround to set the focus on the password field
			txtPassword.addAncestorListener(new AncestorListener() {
				/*
				 * (non-Javadoc)
				 * @see javax.swing.event.AncestorListener#ancestorRemoved(javax.swing.event.AncestorEvent)
				 */
				@Override
				public void ancestorRemoved(AncestorEvent event) {
					// No implementation required!
				}

				/*
				 * (non-Javadoc)
				 * @see javax.swing.event.AncestorListener#ancestorMoved(javax.swing.event.AncestorEvent)
				 */
				@Override
				public void ancestorMoved(AncestorEvent event) {
					// No implementation required!
				}

				/*
				 * (non-Javadoc)
				 * @see javax.swing.event.AncestorListener#ancestorAdded(javax.swing.event.AncestorEvent)
				 */
				@Override
				public void ancestorAdded(AncestorEvent event) {
					final JComponent component = event.getComponent();
					component.requestFocusInWindow();
					component.removeAncestorListener(this);
				}
			});

			if (!hideHostSelection) {
				final ServiceLocatorDTO selHost = hostMap.get(logOnInfo.getHost());

				if (selHost != null)
					cboHost.setSelectedModelObject(selHost);
			}
		}
		else if (!hideHostSelection) {
			// By default, we take the first element of the map to initialize the combobox!
			hostMap.values().stream().findFirst().ifPresent(cboHost::setSelectedModelObject);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#onOKClicked()
	 */
	@Override
	public void onOKClicked() {
		if (txtUserName.getText().isEmpty()) {
			this.setErrorMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_USER));
			return;
		}

		userName = txtUserName.getText();
		password = new String(txtPassword.getPassword());

		if (!hideHostSelection) {
			serviceLocatorDTO = cboHost.getSelectedModelObject();

			if (serviceLocatorDTO == null) {
				this.setErrorMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_HOST));
				return;
			}
		}

		serviceLocatorDTO.setUserName(userName);
		serviceLocatorDTO.setPassword(password);

		try {
			logOn(serviceLocatorDTO);

			setReturnCode(JTitleAreaDialog.RETURN_CODE_OK);

			// Save the input
			LogOnManager.saveLastLogOn(txtUserName.getText(), serviceLocatorDTO.getAlias());

			dispose();
		}
		catch (final SecurityException e) {
			logger.error("Login failed due to bad credentials!", e);

			wrongInputCount++;
			this.setErrorMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_INVALID_CREDENTIALS));

			if (wrongInputCount == MAX_WRONG_INPUTS)
				dispose();
		}
		catch (final Exception e) {
			logger.error("Login failed!", e);

			final String title = getTranslation(ABSTRACT_LOG_ON_DIALOG_TITLE);
			final String message = getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_CONNECT_TO_HOST);

			setErrorMessage(message);
			JOptionPane.showMessageDialog(null, e.getMessage(), title, JOptionPane.WARNING_MESSAGE);
		}
	}

}
