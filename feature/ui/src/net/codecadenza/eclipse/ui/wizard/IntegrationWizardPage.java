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

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_XML_NS;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_XML_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.PREF_INTEGR_CONTEXT_JMS;
import static net.codecadenza.eclipse.shared.Constants.PREF_INTEGR_CONTEXT_KAFKA;
import static net.codecadenza.eclipse.shared.Constants.PREF_INTEGR_CONTEXT_REST;
import static net.codecadenza.eclipse.shared.Constants.PREF_INTEGR_CONTEXT_RMI;
import static net.codecadenza.eclipse.shared.Constants.PREF_INTEGR_CONTEXT_SOAP;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.MappingAnnotationStrategy;
import net.codecadenza.eclipse.model.project.ProjectFactory;
import net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration;
import net.codecadenza.eclipse.model.project.XMLMappingType;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Wizard page for the configuration of integration modules
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationWizardPage extends WizardPage {
	private final ApplicationWizardPage pageApp;
	private final ProjectWizardPage pageProject;
	private Combo cboMappingStrategy;
	private Combo cboMappingType;
	private Button chkSOAP;
	private Button chkREST;
	private Button chkRMI;
	private Button chkKafka;
	private Button chkJMS;
	private Text txtSOAPNamespace;
	private Button chkSOAPHandler;
	private Button chkSOAPClient;
	private Button chkSOAPProducers;
	private Text txtRESTNamespace;
	private Button chkRESTHandler;
	private Button chkRESTClient;
	private Button chkRESTProducers;
	private Text txtRMINamespace;
	private Button chkRMIClient;
	private Button chkRMIProducers;
	private Text txtKafkaNamespace;
	private Button chkKafkaClient;
	private Button chkKafkaProducers;
	private Text txtJMSNamespace;
	private Button chkJMSClient;
	private Button chkJMSProducers;
	private IStatus currentStatus;
	private boolean pageVisible;
	private String xmlNamespace = "";
	private String xmlNamespacePrefix = "";
	private MappingAnnotationStrategy selMappingStrategy;
	private XMLMappingType selMappingType;
	private String soapNamespace;
	private boolean addSOAPClient = true;
	private String restNamespace;
	private boolean addRESTClient = true;
	private String rmiNamespace;
	private boolean addRMIClient = true;
	private String kafkaNamespace;
	private boolean addKafkaClient = true;
	private String jmsNamespace;
	private boolean addJMSClient = true;
	private boolean addSOAPModule;
	private boolean addRESTModule;
	private boolean addRMIModule;
	private boolean addKafkaModule;
	private boolean addJMSModule;
	private List<IntegrationModule> integrationModules = new ArrayList<>();

	/**
	 * Constructor
	 * @param pageNumber
	 * @param pageApp
	 * @param pageProject
	 */
	public IntegrationWizardPage(int pageNumber, ApplicationWizardPage pageApp, ProjectWizardPage pageProject) {
		super("page" + pageNumber);

		this.currentStatus = createStatus(IStatus.OK, "");
		this.pageApp = pageApp;
		this.pageProject = pageProject;

		setTitle("Integration settings");
		setDescription("Definition of integration modules");
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

		final var groupXMLBinding = new Group(panPageArea, SWT.NONE);
		groupXMLBinding.setText("XML binding");
		groupXMLBinding.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupXMLBinding.setLayout(new GridLayout(2, false));

		final var lblXmlNamespace = new Label(groupXMLBinding, SWT.NONE);
		lblXmlNamespace.setText("Namespace name:");

		final var txtXmlNamespaceName = new Text(groupXMLBinding, SWT.BORDER);
		txtXmlNamespaceName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtXmlNamespaceName.addModifyListener(e -> applyXMLNamespace(txtXmlNamespaceName.getText()));
		txtXmlNamespaceName.setText(DEFAULT_XML_NS);

		final var lblXmlNamespacePrefix = new Label(groupXMLBinding, SWT.NONE);
		lblXmlNamespacePrefix.setText("Namespace prefix:");

		final var txtXmlNamespacePrefix = new Text(groupXMLBinding, SWT.BORDER);
		txtXmlNamespacePrefix.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtXmlNamespacePrefix.addModifyListener(e -> xmlNamespacePrefix = txtXmlNamespacePrefix.getText());
		txtXmlNamespacePrefix.setText(DEFAULT_XML_PREFIX);

		final var lblMappingStrategy = new Label(groupXMLBinding, SWT.NONE);
		lblMappingStrategy.setText("Mapping strategy:");

		cboMappingStrategy = new Combo(groupXMLBinding, SWT.READ_ONLY);
		cboMappingStrategy.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		for (final MappingAnnotationStrategy strategy : MappingAnnotationStrategy.values())
			cboMappingStrategy.add(strategy.getName());

		int itemIndex = 0;

		// Set preferred mapping annotation strategy
		for (final String item : cboMappingStrategy.getItems()) {
			if (item.equals(MappingAnnotationStrategy.ON_DEMAND.getName())) {
				cboMappingStrategy.select(itemIndex);
				break;
			}

			itemIndex++;
		}

		cboMappingStrategy.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selMappingStrategy = MappingAnnotationStrategy
						.valueOf(cboMappingStrategy.getItem(cboMappingStrategy.getSelectionIndex()));
			}
		});

		selMappingStrategy = MappingAnnotationStrategy.valueOf(cboMappingStrategy.getItem(cboMappingStrategy.getSelectionIndex()));

		final var lblXMLMappingType = new Label(groupXMLBinding, SWT.NONE);
		lblXMLMappingType.setText("Mapping type:");

		cboMappingType = new Combo(groupXMLBinding, SWT.READ_ONLY);
		cboMappingType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		for (final XMLMappingType strategy : XMLMappingType.values())
			cboMappingType.add(strategy.getName());

		cboMappingType.select(0);

		cboMappingType.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selMappingType = XMLMappingType.valueOf(cboMappingType.getItem(cboMappingType.getSelectionIndex()));
			}
		});

		selMappingType = XMLMappingType.valueOf(cboMappingType.getItem(cboMappingType.getSelectionIndex()));

		final var groupModules = new Group(panPageArea, SWT.NONE);
		groupModules.setText("Module definition");
		groupModules.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupModules.setLayout(new GridLayout(5, false));

		chkSOAP = new Button(groupModules, SWT.CHECK);
		chkSOAP.setText("Add SOAP module");

		chkSOAP.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectSOAPModule(chkSOAP.getSelection());
			}
		});

		chkREST = new Button(groupModules, SWT.CHECK);
		chkREST.setText("Add REST module");

		chkREST.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectRESTModule(chkREST.getSelection());
			}
		});

		chkRMI = new Button(groupModules, SWT.CHECK);
		chkRMI.setText("Add RMI module");

		chkRMI.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectRMIModule(chkRMI.getSelection());
			}
		});

		chkKafka = new Button(groupModules, SWT.CHECK);
		chkKafka.setText("Add Kafka module");

		chkKafka.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectKafkaModule(chkKafka.getSelection());
			}
		});

		chkJMS = new Button(groupModules, SWT.CHECK);
		chkJMS.setText("Add JMS module");

		chkJMS.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectJMSModule(chkJMS.getSelection());
			}
		});

		final var tabFolder = new TabFolder(groupModules, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 5, 1));

		final var tabItemSOAP = new TabItem(tabFolder, SWT.NONE);
		tabItemSOAP.setText("SOAP");

		final var panSOAP = new Composite(tabFolder, SWT.NONE);
		panSOAP.setLayout(new GridLayout(2, false));

		final var lblSOAPNamespace = new Label(panSOAP, SWT.NONE);
		lblSOAPNamespace.setText("Package:");

		txtSOAPNamespace = new Text(panSOAP, SWT.BORDER);
		txtSOAPNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtSOAPNamespace.addModifyListener(e -> validateSOAPNamespace(txtSOAPNamespace.getText()));

		final var lblSOAPHandler = new Label(panSOAP, SWT.NONE);
		lblSOAPHandler.setText("Add security handler:");

		chkSOAPHandler = new Button(panSOAP, SWT.CHECK);
		chkSOAPHandler.setSelection(true);

		chkSOAPHandler.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		final var lblSOAPClient = new Label(panSOAP, SWT.NONE);
		lblSOAPClient.setText("Add client:");

		chkSOAPClient = new Button(panSOAP, SWT.CHECK);
		chkSOAPClient.setSelection(true);

		chkSOAPClient.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		final var lblSOAPProducers = new Label(panSOAP, SWT.NONE);
		lblSOAPProducers.setText("Add producers:");

		chkSOAPProducers = new Button(panSOAP, SWT.CHECK);
		chkSOAPProducers.setSelection(true);

		chkSOAPProducers.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		tabItemSOAP.setControl(panSOAP);

		final var tabItemREST = new TabItem(tabFolder, SWT.NONE);
		tabItemREST.setText("REST");

		final var panREST = new Composite(tabFolder, SWT.NONE);
		panREST.setLayout(new GridLayout(2, false));

		final var lblRESTNamespace = new Label(panREST, SWT.NONE);
		lblRESTNamespace.setText("Package:");

		txtRESTNamespace = new Text(panREST, SWT.BORDER);
		txtRESTNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtRESTNamespace.addModifyListener(e -> validateRESTNamespace(txtRESTNamespace.getText()));

		final var lblRESTHandler = new Label(panREST, SWT.NONE);
		lblRESTHandler.setText("Add security handler:");

		chkRESTHandler = new Button(panREST, SWT.CHECK);
		chkRESTHandler.setSelection(true);

		chkRESTHandler.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		final var lblRESTClient = new Label(panREST, SWT.NONE);
		lblRESTClient.setText("Add client:");

		chkRESTClient = new Button(panREST, SWT.CHECK);
		chkRESTClient.setSelection(true);

		chkRESTClient.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		final var lblRESTProducers = new Label(panREST, SWT.NONE);
		lblRESTProducers.setText("Add producers:");

		chkRESTProducers = new Button(panREST, SWT.CHECK);
		chkRESTProducers.setSelection(true);

		chkRESTProducers.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		tabItemREST.setControl(panREST);

		final var tabItemRMI = new TabItem(tabFolder, SWT.NONE);
		tabItemRMI.setText("RMI");

		final var panRMI = new Composite(tabFolder, SWT.NONE);
		panRMI.setLayout(new GridLayout(2, false));

		final var lblRMINamespace = new Label(panRMI, SWT.NONE);
		lblRMINamespace.setText("Package:");

		txtRMINamespace = new Text(panRMI, SWT.BORDER);
		txtRMINamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtRMINamespace.addModifyListener(e -> validateRMINamespace(txtRMINamespace.getText()));

		final var lblRMIClient = new Label(panRMI, SWT.NONE);
		lblRMIClient.setText("Add client:");

		chkRMIClient = new Button(panRMI, SWT.CHECK);
		chkRMIClient.setSelection(true);

		chkRMIClient.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		final var lblRMIProducers = new Label(panRMI, SWT.NONE);
		lblRMIProducers.setText("Add producers:");

		chkRMIProducers = new Button(panRMI, SWT.CHECK);
		chkRMIProducers.setSelection(true);

		chkRMIProducers.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		tabItemRMI.setControl(panRMI);

		final var tabItemKafka = new TabItem(tabFolder, SWT.NONE);
		tabItemKafka.setText("Kafka");

		final var panKafka = new Composite(tabFolder, SWT.NONE);
		panKafka.setLayout(new GridLayout(2, false));

		final var lblKafkaNamespace = new Label(panKafka, SWT.NONE);
		lblKafkaNamespace.setText("Package:");

		txtKafkaNamespace = new Text(panKafka, SWT.BORDER);
		txtKafkaNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtKafkaNamespace.addModifyListener(e -> validateKafkaNamespace(txtKafkaNamespace.getText()));

		final var lblKafkaClient = new Label(panKafka, SWT.NONE);
		lblKafkaClient.setText("Add client:");

		chkKafkaClient = new Button(panKafka, SWT.CHECK);
		chkKafkaClient.setSelection(true);

		chkKafkaClient.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		final var lblKafkaProducers = new Label(panKafka, SWT.NONE);
		lblKafkaProducers.setText("Add producers:");

		chkKafkaProducers = new Button(panKafka, SWT.CHECK);
		chkKafkaProducers.setSelection(true);

		chkKafkaProducers.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		tabItemKafka.setControl(panKafka);

		final var tabItemJMS = new TabItem(tabFolder, SWT.NONE);
		tabItemJMS.setText("JMS");

		final var panJMS = new Composite(tabFolder, SWT.NONE);
		panJMS.setLayout(new GridLayout(2, false));

		final var lblJMSNamespace = new Label(panJMS, SWT.NONE);
		lblJMSNamespace.setText("Package:");

		txtJMSNamespace = new Text(panJMS, SWT.BORDER);
		txtJMSNamespace.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtJMSNamespace.addModifyListener(e -> validateJMSNamespace(txtJMSNamespace.getText()));

		final var lblJMSClient = new Label(panJMS, SWT.NONE);
		lblJMSClient.setText("Add client:");

		chkJMSClient = new Button(panJMS, SWT.CHECK);
		chkJMSClient.setSelection(true);

		chkJMSClient.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		final var lblJMSProducers = new Label(panJMS, SWT.NONE);
		lblJMSProducers.setText("Add producers:");

		chkJMSProducers = new Button(panJMS, SWT.CHECK);
		chkJMSProducers.setSelection(true);

		chkJMSProducers.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				onInputChanged();
			}
		});

		tabItemJMS.setControl(panJMS);

		selectSOAPModule(false);
		selectRESTModule(false);
		selectRMIModule(false);
		selectKafkaModule(false);
		selectJMSModule(false);

		// Get the preference store
		final IPreferenceStore store = CodeCadenzaUserInterfacePlugin.getInstance().getPreferenceStore();

		txtSOAPNamespace.setText(store.getString(PREF_INTEGR_CONTEXT_SOAP));
		txtRESTNamespace.setText(store.getString(PREF_INTEGR_CONTEXT_REST));
		txtRMINamespace.setText(store.getString(PREF_INTEGR_CONTEXT_RMI));
		txtKafkaNamespace.setText(store.getString(PREF_INTEGR_CONTEXT_KAFKA));
		txtJMSNamespace.setText(store.getString(PREF_INTEGR_CONTEXT_JMS));
	}

	/**
	 * Method that must be called as soon as integration module data has been changed
	 */
	private void onInputChanged() {
		addSOAPClient = chkSOAPClient.getSelection();
		addRESTClient = chkRESTClient.getSelection();
		addRMIClient = chkRMIClient.getSelection();
		addKafkaClient = chkKafkaClient.getSelection();
		addJMSClient = chkJMSClient.getSelection();

		final boolean addSOAPProducers = addSOAPClient && chkSOAPProducers.getSelection();
		final boolean addRESTProducers = addRESTClient && chkRESTProducers.getSelection();
		final boolean addRMIProducers = addRMIClient && chkRMIProducers.getSelection();
		final boolean addKafkaProducers = addKafkaClient && chkKafkaProducers.getSelection();
		final boolean addJMSProducers = addJMSClient && chkJMSProducers.getSelection();

		chkSOAPProducers.setEnabled(addSOAPClient && addSOAPModule);
		chkRESTProducers.setEnabled(addRESTClient && addRESTModule);
		chkRMIProducers.setEnabled(addRMIClient && addRMIModule);
		chkKafkaProducers.setEnabled(addKafkaClient && addKafkaModule);
		chkJMSProducers.setEnabled(addJMSClient && addJMSModule);

		// Remove all existing entries!
		integrationModules = new ArrayList<>();

		if (addSOAPModule)
			integrationModules
					.add(initModule(IntegrationTechnology.SOAP, chkSOAPHandler.getSelection(), addSOAPProducers, soapNamespace));

		if (addRESTModule)
			integrationModules
					.add(initModule(IntegrationTechnology.REST, chkRESTHandler.getSelection(), addRESTProducers, restNamespace));

		if (addRMIModule)
			integrationModules.add(initModule(IntegrationTechnology.RMI, false, addRMIProducers, rmiNamespace));

		if (addKafkaModule)
			integrationModules.add(initModule(IntegrationTechnology.KAFKA, false, addKafkaProducers, kafkaNamespace));

		if (addJMSModule)
			integrationModules.add(initModule(IntegrationTechnology.JMS, false, addJMSProducers, jmsNamespace));
	}

	/**
	 * Initialize an integration module by using the given parameters
	 * @param technology
	 * @param addHandler
	 * @param addProducers
	 * @param namespaceName
	 * @return the new integration module
	 */
	private IntegrationModule initModule(IntegrationTechnology technology, boolean addHandler, boolean addProducers,
			String namespaceName) {
		final Namespace integrationNamespace = JavaFactory.eINSTANCE.createNamespace();
		integrationNamespace.setName(namespaceName);

		final IntegrationModule integrationModule = ProjectFactory.eINSTANCE.createIntegrationModule();
		integrationModule.setAddSecurityHandler(addHandler);
		integrationModule.setNamespace(integrationNamespace);
		integrationModule.setTechnology(technology);
		integrationModule.setAddProducers(addProducers);

		return integrationModule;
	}

	/**
	 * @param text
	 */
	private void applyXMLNamespace(String text) {
		IStatus status = createStatus(IStatus.INFO, "XML namespace entered");

		if (text.isEmpty())
			status = createStatus(IStatus.ERROR, "A XML namespace must be entered!");

		updateStatus(status);

		xmlNamespace = text;
	}

	/**
	 * @param selection
	 */
	private void selectSOAPModule(boolean selection) {
		txtSOAPNamespace.setEnabled(selection);
		chkSOAPClient.setEnabled(selection);
		chkSOAPHandler.setEnabled(selection);

		addSOAPModule = selection;

		onInputChanged();
	}

	/**
	 * @param selection
	 */
	private void selectRESTModule(boolean selection) {
		txtRESTNamespace.setEnabled(selection);
		chkRESTClient.setEnabled(selection);
		chkRESTHandler.setEnabled(selection);

		addRESTModule = selection;

		onInputChanged();
	}

	/**
	 * @param selection
	 */
	private void selectRMIModule(boolean selection) {
		txtRMINamespace.setEnabled(selection);
		chkRMIClient.setEnabled(selection);

		addRMIModule = selection;

		onInputChanged();
	}

	/**
	 * @param selection
	 */
	private void selectKafkaModule(boolean selection) {
		txtKafkaNamespace.setEnabled(selection);
		chkKafkaClient.setEnabled(selection);

		addKafkaModule = selection;

		onInputChanged();
	}

	/**
	 * @param selection
	 */
	private void selectJMSModule(boolean selection) {
		txtJMSNamespace.setEnabled(selection);
		chkJMSClient.setEnabled(selection);

		addJMSModule = selection;

		onInputChanged();
	}

	/**
	 * Validate the SOAP namespace
	 * @param text
	 */
	private void validateSOAPNamespace(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		soapNamespace = text;

		onInputChanged();
	}

	/**
	 * @param text
	 */
	private void validateRESTNamespace(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		restNamespace = text;

		onInputChanged();
	}

	/**
	 * Validate the RMI namespace
	 * @param text
	 */
	private void validateRMINamespace(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		rmiNamespace = text;

		onInputChanged();
	}

	/**
	 * Validate the Kafka namespace
	 * @param text
	 */
	private void validateKafkaNamespace(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		kafkaNamespace = text;

		onInputChanged();
	}

	/**
	 * Validate the JMS namespace
	 * @param text
	 */
	private void validateJMSNamespace(String text) {
		final IStatus status = EclipseIDEService.validatePackageName(text);
		updateStatus(status);

		jmsNamespace = text;

		onInputChanged();
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

		if (visible) {
			boolean enableModules = true;

			if (pageApp.getTechnologyPlatform() == TechnologyPlatformEnumeration.JAVA_SE)
				enableModules = false;

			chkSOAP.setEnabled(enableModules);
			chkSOAP.setSelection(false);
			chkREST.setEnabled(enableModules);
			chkREST.setSelection(false);
			chkJMS.setEnabled(enableModules);
			chkJMS.setSelection(false);

			// The REST interface is mandatory for Angular applications!
			if (pageApp.getClientPlatform() == ClientPlatformEnumeration.ANGULAR) {
				chkREST.setEnabled(false);
				chkREST.setSelection(true);

				selectRESTModule(true);
			}
			else
				selectRESTModule(false);

			// We don't support RMI for Spring!
			chkRMI.setEnabled(pageApp.getTechnologyPlatform() == TechnologyPlatformEnumeration.JAKARTA_EE);
			chkRMI.setSelection(false);

			// Kafka is only supported for Spring Boot applications!
			chkKafka.setEnabled(
					pageApp.getTechnologyPlatform() == TechnologyPlatformEnumeration.SPRING_BOOT && pageProject.isBoundaryMode());
			chkKafka.setSelection(false);

			selectSOAPModule(false);
			selectRMIModule(false);
			selectKafkaModule(false);
			selectJMSModule(false);
		}

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
	 * @return the XML namespace
	 */
	public String getXmlNamespace() {
		return xmlNamespace;
	}

	/**
	 * @return the XML namespace prefix
	 */
	public String getXmlNamespacePrefix() {
		return xmlNamespacePrefix;
	}

	/**
	 * @return the selected mapping strategy
	 */
	public MappingAnnotationStrategy getMappingStrategy() {
		return selMappingStrategy;
	}

	/**
	 * @return the selected XML mapping type
	 */
	public XMLMappingType getXMLMappingType() {
		return selMappingType;
	}

	/**
	 * @return true if a SOAP client should be generated
	 */
	public boolean isAddSOAPClient() {
		return addSOAPClient;
	}

	/**
	 * @return true if a REST client should be generated
	 */
	public boolean isAddRESTClient() {
		return addRESTClient;
	}

	/**
	 * @return true if an RMI client should be generated
	 */
	public boolean isAddRMIClient() {
		return addRMIClient;
	}

	/**
	 * @return true if a Kafka client should be generated
	 */
	public boolean isAddKafkaClient() {
		return addKafkaClient;
	}

	/**
	 * @return true if a JMS client should be generated
	 */
	public boolean isAddJMSClient() {
		return addJMSClient;
	}

	/**
	 * @return true if a SOAP module should be created
	 */
	public boolean isAddSOAPModule() {
		return addSOAPModule;
	}

	/**
	 * @return true if a REST module should be created
	 */
	public boolean isAddRESTModule() {
		return addRESTModule;
	}

	/**
	 * @return true if an RMI module should be created
	 */
	public boolean isAddRMIModule() {
		return addRMIModule;
	}

	/**
	 * @return true if a Kafka module should be created
	 */
	public boolean isAddKafkaModule() {
		return addKafkaModule;
	}

	/**
	 * @return true if a JMS module should be created
	 */
	public boolean isAddJMSModule() {
		return addJMSModule;
	}

	/**
	 * @return a list containing all integration modules that has been defined by the user
	 */
	public List<IntegrationModule> getIntegrationModules() {
		return integrationModules;
	}

	/**
	 * @param technology
	 * @return an integration module for the given technology
	 */
	public IntegrationModule getIntegrationModule(IntegrationTechnology technology) {
		return integrationModules.stream().filter(i -> i.getTechnology() == technology).findFirst().orElse(null);
	}

}
