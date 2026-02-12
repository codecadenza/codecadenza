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
package net.codecadenza.eclipse.ui.wizard;

import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.project.JPAVersionEnumeration;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.ServerPlatformEnumeration;
import net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Wizard page for client and server-specific settings
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ApplicationWizardPage extends WizardPage {
	private static final String DEFAULT_CLIENT_NAMESPACE_NAME = "ui";

	private Text txtDeployDir;
	private Text txtNamespace;
	private Combo cboServerPlatform;
	private Combo cboPersistenceProvider;
	private Combo cboJPAVersion;
	private Combo cboTechnology;
	private Combo cboValidationStrategy;
	private Combo cboImp;
	private IStatus currentStatus;
	private boolean pageVisible;
	private String clientNamespace = "";
	private ClientPlatformEnumeration selClientPlatform;
	private ServerPlatformEnumeration selServerPlatform;
	private PersistenceProviderEnumeration selPersistenceProvider;
	private TechnologyPlatformEnumeration selTechnology;
	private ValidationTypeEnumeration selValidation;
	private JPAVersionEnumeration selJPAVersion;
	private String deployDir = "";
	private Button cmdBrowse;
	private Label lblDeployDir;

	/**
	 * Constructor
	 * @param pageNumber
	 */
	public ApplicationWizardPage(int pageNumber) {
		super("page" + pageNumber);

		this.currentStatus = createStatus(IStatus.OK, "");

		setTitle("Application settings");
		setDescription("Enter data for client & server");
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createControl(Composite parent) {
		final var panPageArea = new Composite(parent, SWT.NONE);
		panPageArea.setLayout(new GridLayout());

		setControl(panPageArea);

		final var groupClient = new Group(panPageArea, SWT.NONE);
		groupClient.setText("Client");
		groupClient.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupClient.setLayout(new GridLayout(2, false));

		final var lblImpType = new Label(groupClient, SWT.NONE);
		lblImpType.setText("Implementation:");

		cboImp = new Combo(groupClient, SWT.READ_ONLY);
		cboImp.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		for (final ClientPlatformEnumeration platform : ClientPlatformEnumeration.values())
			cboImp.add(platform.name());

		cboImp.select(0);

		cboImp.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selClientPlatform = ClientPlatformEnumeration.valueOf(cboImp.getItem(cboImp.getSelectionIndex()));
				cboServerPlatform.removeAll();
				lblDeployDir.setEnabled(true);
				cmdBrowse.setEnabled(true);

				cboServerPlatform.add(ServerPlatformEnumeration.PAYARA.name());
				cboServerPlatform.add(ServerPlatformEnumeration.WILDFLY.name());
				cboServerPlatform.add(ServerPlatformEnumeration.TOMCAT.name());

				if (selClientPlatform == ClientPlatformEnumeration.RCP || selClientPlatform == ClientPlatformEnumeration.SWING
						|| selClientPlatform == ClientPlatformEnumeration.JAVAFX)
					cboServerPlatform.add(ServerPlatformEnumeration.NONE.name());

				txtNamespace.setEnabled(selClientPlatform != ClientPlatformEnumeration.NONE);

				cboServerPlatform.select(0);
				selServerPlatform = ServerPlatformEnumeration.valueOf(cboServerPlatform.getItem(cboServerPlatform.getSelectionIndex()));

				setValidTechnologies();
			}
		});

		selClientPlatform = ClientPlatformEnumeration.valueOf(cboImp.getItem(cboImp.getSelectionIndex()));

		final var lblNamespace = new Label(groupClient, SWT.NONE);
		lblNamespace.setText("Client package:");

		txtNamespace = new Text(groupClient, SWT.BORDER);
		txtNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		txtNamespace.setText(DEFAULT_CLIENT_NAMESPACE_NAME);
		txtNamespace.setEnabled(false);

		clientNamespace = txtNamespace.getText();

		txtNamespace.addModifyListener(_ -> validateNamespace(txtNamespace.getText()));

		final var groupServer = new Group(panPageArea, SWT.NONE);
		groupServer.setText("Server");
		groupServer.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupServer.setLayout(new GridLayout(2, false));

		final var lblPlatform = new Label(groupServer, SWT.NONE);
		lblPlatform.setText("Platform:");

		cboServerPlatform = new Combo(groupServer, SWT.READ_ONLY);
		cboServerPlatform.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboServerPlatform.add(ServerPlatformEnumeration.PAYARA.name());
		cboServerPlatform.add(ServerPlatformEnumeration.WILDFLY.name());
		cboServerPlatform.add(ServerPlatformEnumeration.TOMCAT.name());
		cboServerPlatform.select(0);

		cboServerPlatform.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selServerPlatform = ServerPlatformEnumeration.valueOf(cboServerPlatform.getItem(cboServerPlatform.getSelectionIndex()));

				setValidTechnologies();
				adaptDeploymentControls();
			}
		});

		selServerPlatform = ServerPlatformEnumeration.valueOf(cboServerPlatform.getItem(cboServerPlatform.getSelectionIndex()));

		final var lblTechnology = new Label(groupServer, SWT.NONE);
		lblTechnology.setText("Technology:");

		cboTechnology = new Combo(groupServer, SWT.READ_ONLY);
		cboTechnology.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboTechnology.add(TechnologyPlatformEnumeration.JAKARTA_EE.name());
		cboTechnology.select(0);

		cboTechnology.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selTechnology = TechnologyPlatformEnumeration.valueOf(cboTechnology.getItem(cboTechnology.getSelectionIndex()));

				adaptDeploymentControls();
				setValidPersistenceProviders();
				setValidJPAVersions();
			}
		});

		selTechnology = TechnologyPlatformEnumeration.valueOf(cboTechnology.getItem(cboTechnology.getSelectionIndex()));

		lblDeployDir = new Label(groupServer, SWT.NONE);
		lblDeployDir.setText("Deploy directory:");

		final var glPanBrowse = new GridLayout(2, false);
		glPanBrowse.marginHeight = 0;
		glPanBrowse.marginWidth = 0;
		glPanBrowse.horizontalSpacing = 0;

		final var panBrowse = new Composite(groupServer, SWT.NONE);
		panBrowse.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		panBrowse.setLayout(glPanBrowse);

		txtDeployDir = new Text(panBrowse, SWT.BORDER);
		txtDeployDir.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));
		txtDeployDir.setEditable(false);
		txtDeployDir.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		cmdBrowse = new Button(panBrowse, SWT.NONE);
		cmdBrowse.setText("Browse...");

		cmdBrowse.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var dlg = new DirectoryDialog(getShell());
				dlg.setText("Select a deploy directory");

				final String dir = dlg.open();

				if (dir != null) {
					txtDeployDir.setText(dir);
					deployDir = txtDeployDir.getText();
				}
			}
		});

		final var groupPersistence = new Group(panPageArea, SWT.NONE);
		groupPersistence.setText("Persistence");
		groupPersistence.setLayout(new GridLayout(2, false));
		groupPersistence.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblProvider = new Label(groupPersistence, SWT.NONE);
		lblProvider.setText("Persistence provider:");

		cboPersistenceProvider = new Combo(groupPersistence, SWT.READ_ONLY);
		cboPersistenceProvider.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		setValidPersistenceProviders();

		cboPersistenceProvider.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selPersistenceProvider = PersistenceProviderEnumeration
						.valueOf(cboPersistenceProvider.getItem(cboPersistenceProvider.getSelectionIndex()));
			}
		});

		final var lblJPAVersion = new Label(groupPersistence, SWT.NONE);
		lblJPAVersion.setText("JPA version:");

		cboJPAVersion = new Combo(groupPersistence, SWT.READ_ONLY);
		cboJPAVersion.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboJPAVersion.add(JPAVersionEnumeration.JPA3.name());
		cboJPAVersion.select(0);

		cboJPAVersion.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selJPAVersion = JPAVersionEnumeration.valueOf(cboJPAVersion.getItem(cboJPAVersion.getSelectionIndex()));
			}
		});

		selJPAVersion = JPAVersionEnumeration.valueOf(cboJPAVersion.getItem(cboJPAVersion.getSelectionIndex()));

		final var lblValidation = new Label(groupPersistence, SWT.NONE);
		lblValidation.setText("Validation type:");

		cboValidationStrategy = new Combo(groupPersistence, SWT.READ_ONLY);
		cboValidationStrategy.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboValidationStrategy.add(ValidationTypeEnumeration.STANDARD.name());
		cboValidationStrategy.add(ValidationTypeEnumeration.INTERNAL.name());
		cboValidationStrategy.select(0);

		cboValidationStrategy.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selValidation = ValidationTypeEnumeration
						.valueOf(cboValidationStrategy.getItem(cboValidationStrategy.getSelectionIndex()));
			}
		});

		selValidation = ValidationTypeEnumeration.valueOf(cboValidationStrategy.getItem(cboValidationStrategy.getSelectionIndex()));

		validateNamespace(clientNamespace);
	}

	/**
	 * @return true if the facade mode is supported by the given configuration
	 */
	public boolean isFacadeModeSupported() {
		// The facade mode isn't supported in a JSF or Vaadin application that uses Hibernate to avoid lazy-load exceptions!
		return (selTechnology == TechnologyPlatformEnumeration.JAVA_SE || selClientPlatform == ClientPlatformEnumeration.NONE)
				|| ((selClientPlatform == ClientPlatformEnumeration.JSF_PRIMEFACES
						|| selClientPlatform == ClientPlatformEnumeration.VAADIN)
						&& selPersistenceProvider != PersistenceProviderEnumeration.HIBERNATE);
	}

	/**
	 * Set valid technologies due to the selected client and server platform
	 */
	private void setValidTechnologies() {
		cboTechnology.removeAll();

		if (selServerPlatform == ServerPlatformEnumeration.NONE)
			cboTechnology.add(TechnologyPlatformEnumeration.JAVA_SE.name());
		else if (selServerPlatform == ServerPlatformEnumeration.TOMCAT) {
			if (selClientPlatform == ClientPlatformEnumeration.RAP)
				cboTechnology.add(TechnologyPlatformEnumeration.JAVA_SE.name());

			cboTechnology.add(TechnologyPlatformEnumeration.SPRING_BOOT.name());
		}
		else
			cboTechnology.add(TechnologyPlatformEnumeration.JAKARTA_EE.name());

		cboTechnology.select(0);
		selTechnology = TechnologyPlatformEnumeration.valueOf(cboTechnology.getItem(cboTechnology.getSelectionIndex()));

		setValidPersistenceProviders();
		setValidJPAVersions();
	}

	/**
	 * Set valid JPA versions due to the selected technology
	 */
	private void setValidJPAVersions() {
		cboJPAVersion.removeAll();
		cboJPAVersion.add(JPAVersionEnumeration.JPA3.name());
		cboJPAVersion.select(0);

		selJPAVersion = JPAVersionEnumeration.valueOf(cboJPAVersion.getItem(cboJPAVersion.getSelectionIndex()));
	}

	/**
	 * Set valid persistence providers
	 */
	public void setValidPersistenceProviders() {
		cboPersistenceProvider.removeAll();

		if (selTechnology == TechnologyPlatformEnumeration.JAVA_SE)
			cboPersistenceProvider.add(PersistenceProviderEnumeration.ECLIPSELINK.name());
		else if (selServerPlatform == ServerPlatformEnumeration.PAYARA) {
			cboPersistenceProvider.add(PersistenceProviderEnumeration.ECLIPSELINK.name());
			cboPersistenceProvider.add(PersistenceProviderEnumeration.HIBERNATE.name());
		}
		else {
			cboPersistenceProvider.add(PersistenceProviderEnumeration.HIBERNATE.name());
			cboPersistenceProvider.add(PersistenceProviderEnumeration.ECLIPSELINK.name());
		}

		cboPersistenceProvider.select(0);
		selPersistenceProvider = PersistenceProviderEnumeration
				.valueOf(cboPersistenceProvider.getItem(cboPersistenceProvider.getSelectionIndex()));
	}

	/**
	 * Disable or enable the controls for selecting the deploy directory depending on the selected platform and technology
	 */
	private void adaptDeploymentControls() {
		lblDeployDir.setEnabled(true);
		cmdBrowse.setEnabled(true);

		if (selServerPlatform == ServerPlatformEnumeration.NONE || selTechnology == TechnologyPlatformEnumeration.SPRING_BOOT) {
			// In case of a Java SE or a Spring Boot application, it makes no sense to define a deploy directory!
			lblDeployDir.setEnabled(false);
			cmdBrowse.setEnabled(false);
			txtDeployDir.setText("");
			deployDir = "";
		}
	}

	/**
	 * Validate the client namespace name
	 * @param text
	 */
	private void validateNamespace(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		clientNamespace = text;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.DialogPage#setVisible(boolean)
	 */
	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		pageVisible = visible;

		if (visible && currentStatus.matches(IStatus.ERROR))
			currentStatus = createStatus(IStatus.ERROR, "");

		updateStatus(currentStatus);
	}

	/**
	 * Update the status line and the 'Finish' button depending on the status
	 * @param status
	 */
	private void updateStatus(IStatus status) {
		currentStatus = status;
		setPageComplete(!status.matches(IStatus.ERROR));

		if (pageVisible)
			applyToStatusLine(this, status);
	}

	/**
	 * @param page
	 * @param status
	 */
	private static void applyToStatusLine(DialogPage page, IStatus status) {
		String errorMessage = null;
		String warningMessage = null;
		final String statusMessage = status.getMessage();

		if (!statusMessage.isEmpty()) {
			if (status.matches(IStatus.ERROR))
				errorMessage = statusMessage;
			else if (!status.isOK())
				warningMessage = statusMessage;
		}

		page.setErrorMessage(errorMessage);
		page.setMessage(warningMessage);
	}

	/**
	 * @param severity
	 * @param message
	 * @return the status
	 */
	private static IStatus createStatus(int severity, String message) {
		return new Status(severity, CodeCadenzaUserInterfacePlugin.PLUGIN_ID, severity, message, null);
	}

	/**
	 * @return the client context
	 */
	public String getClientNamespace() {
		return clientNamespace;
	}

	/**
	 * @return the selected client platform type
	 */
	public ClientPlatformEnumeration getClientPlatform() {
		return selClientPlatform;
	}

	/**
	 * @return the selected server platform
	 */
	public ServerPlatformEnumeration getServerPlatform() {
		return selServerPlatform;
	}

	/**
	 * @return the selected validation type
	 */
	public ValidationTypeEnumeration getValidationType() {
		return selValidation;
	}

	/**
	 * @return the selected technology platform
	 */
	public TechnologyPlatformEnumeration getTechnologyPlatform() {
		return selTechnology;
	}

	/**
	 * @return the deploy directory
	 */
	public String getDeployDir() {
		return deployDir;
	}

	/**
	 * @return the selected persistence provider
	 */
	public PersistenceProviderEnumeration getPersistenceProvider() {
		return selPersistenceProvider;
	}

	/**
	 * @return the selected JPA version
	 */
	public JPAVersionEnumeration getJPAVersion() {
		return selJPAVersion;
	}

}
