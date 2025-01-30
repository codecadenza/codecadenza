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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOG_ON_DIALOG_INFO_MESSAGE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOG_ON_DIALOG_LBL_HOST;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOG_ON_DIALOG_LBL_PASSWORD;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOG_ON_DIALOG_LBL_USER_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOG_ON_DIALOG_MSG_CONNECT_TO_HOST;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOG_ON_DIALOG_MSG_INVALID_CREDENTIALS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_HOST;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_USER;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_LOG_ON_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_CANCEL;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_OK;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslationForFieldLabel;

import java.lang.invoke.MethodHandles;
import java.util.Collection;
import java.util.Map;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.persistence.entity.LastLogOn;
import net.codecadenza.runtime.richclient.transport.ServiceLocatorDTO;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
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
public abstract class __AbstractLogOnDialog extends TitleAreaDialog {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private Text txtPassword;
	private Text txtUserName;
	private final Map<String, ServiceLocatorDTO> hostMap;
	private ComboViewer comboHost;
	private int wrongInputCount;
	private static final int MAX_WRONG_INPUTS = 3;
	private String userName = "";
	private String password = "";
	private ServiceLocatorDTO serviceLocatorDTO;
	private boolean hideHostSelection;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param hostMap a map that contains all available connection configurations
	 */
	protected __AbstractLogOnDialog(Shell parentShell, Map<String, ServiceLocatorDTO> hostMap) {
		super(parentShell);

		this.hostMap = hostMap;

		// If the caller just gives us a list containing one element we will hide the respective selection component!
		if (hostMap.size() == 1) {
			hideHostSelection = true;

			hostMap.values().stream().findFirst().ifPresent(dto -> serviceLocatorDTO = dto);
		}
	}

	/**
	 * @return the information about the last logged on user
	 */
	protected abstract LastLogOn getLastLogOn();

	/**
	 * @param userName
	 * @param alias
	 */
	protected abstract void saveLastLogOn(String userName, String alias);

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

	/**
	 * List label provider
	 */
	class HostListLabelProvider extends LabelProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
		 */
		@Override
		public String getText(Object element) {
			final var host = (ServiceLocatorDTO) element;

			return host.getAlias();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
		 */
		@Override
		public Image getImage(Object element) {
			return null;
		}
	}

	/**
	 * Content provider
	 */
	class HostListContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Object[] getElements(Object inputElement) {
			return ((Collection<ServiceLocatorDTO>) inputElement).toArray();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// No implementation required!
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var panMain = new Composite(panDialogArea, SWT.NONE);
		panMain.setLayout(new GridLayout(2, false));
		panMain.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var lblUserName = new Label(panMain, SWT.NONE);
		lblUserName.setText(getTranslationForFieldLabel(ABSTRACT_LOG_ON_DIALOG_LBL_USER_NAME));

		txtUserName = new Text(panMain, SWT.BORDER);
		txtUserName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final LastLogOn logOn = getLastLogOn();

		if (logOn != null)
			txtUserName.setText(logOn.getUserName());

		final var lblPassword = new Label(panMain, SWT.NONE);
		lblPassword.setText(getTranslationForFieldLabel(ABSTRACT_LOG_ON_DIALOG_LBL_PASSWORD));

		txtPassword = new Text(panMain, SWT.PASSWORD | SWT.BORDER);
		txtPassword.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (!hideHostSelection) {
			final var lblHost = new Label(panMain, SWT.NONE);
			lblHost.setText(getTranslationForFieldLabel(ABSTRACT_LOG_ON_DIALOG_LBL_HOST));

			comboHost = new ComboViewer(panMain, SWT.READ_ONLY);
			comboHost.setLabelProvider(new HostListLabelProvider());
			comboHost.setContentProvider(new HostListContentProvider());
			comboHost.setInput(hostMap.values());

			if (logOn != null) {
				if (hostMap.containsKey(logOn.getHost())) {
					final ServiceLocatorDTO host = hostMap.get(logOn.getHost());
					final var sel = new StructuredSelection(host);

					comboHost.setSelection(sel, true);
				}
			}
			else {
				// By default, we take the first element of the map to initialize the combobox!
				hostMap.values().stream().findFirst().ifPresent(host -> comboHost.setSelection(new StructuredSelection(host), true));
			}

			final Combo cboHost = comboHost.getCombo();
			cboHost.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}

		setTitle(getTranslation(ABSTRACT_LOG_ON_DIALOG_INFO_MESSAGE));
		setTitleImage(ImageCache.getImage("logon.png"));

		if (txtUserName.getText().isEmpty())
			txtUserName.setFocus();
		else
			txtPassword.setFocus();

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, getTranslation(CMD_OK), true);
		createButton(parent, IDialogConstants.CANCEL_ID, getTranslation(CMD_CANCEL), false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(getTranslation(ABSTRACT_LOG_ON_DIALOG_TITLE));
	}

	/**
	 * @throws SecurityException if the login has failed
	 */
	public abstract void logOn();

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the input
			if (txtUserName.getText().isEmpty()) {
				setErrorMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_USER));
				return;
			}

			userName = txtUserName.getText();
			password = txtPassword.getText();

			if (!hideHostSelection) {
				final var hostSelection = (StructuredSelection) comboHost.getSelection();
				serviceLocatorDTO = (ServiceLocatorDTO) hostSelection.getFirstElement();

				if (serviceLocatorDTO == null) {
					setErrorMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_HOST));
					return;
				}
			}

			serviceLocatorDTO.setUserName(userName);
			serviceLocatorDTO.setPassword(password);

			try {
				logOn();

				// Save the input
				saveLastLogOn(txtUserName.getText(), serviceLocatorDTO.getAlias());

				setReturnCode(Dialog.OK);
				close();
			}
			catch (final SecurityException e) {
				logger.error("Login failed due to bad credentials!", e);

				wrongInputCount++;
				setErrorMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_INVALID_CREDENTIALS));

				if (wrongInputCount == MAX_WRONG_INPUTS) {
					setReturnCode(Dialog.CANCEL);
					close();
				}
			}
			catch (final Exception e) {
				logger.error("Login failed!", e);

				final String title = getTranslation(ABSTRACT_LOG_ON_DIALOG_TITLE);
				final String message = getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_CONNECT_TO_HOST);

				setErrorMessage(message);
				MessageDialog.openWarning(Display.getCurrent().getActiveShell(), title, e.getMessage());
			}

			return;
		}

		super.buttonPressed(buttonId);
	}

}
