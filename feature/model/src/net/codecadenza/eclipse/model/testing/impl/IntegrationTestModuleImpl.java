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
package net.codecadenza.eclipse.model.testing.impl;

import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Integration Test Module</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl#getDefaultTimeout <em>Default
 * Timeout</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl#getDecimalFormat <em>Decimal Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl#getDateTimeFormat <em>Date Time
 * Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl#getIntegrationModule <em>Integration
 * Module</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl#getDateFormat <em>Date Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl#getDecimalSeparator <em>Decimal
 * Separator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.IntegrationTestModuleImpl#getGroupingSeparator <em>Grouping
 * Separator</em>}</li>
 * </ul>
 * @generated
 */
public class IntegrationTestModuleImpl extends AbstractTestModuleImpl implements IntegrationTestModule {
	/**
	 * The default value of the '{@link #getDefaultTimeout() <em>Default Timeout</em>}' attribute
	 * @see #getDefaultTimeout()
	 * @generated
	 * @ordered
	 */
	protected static final Integer DEFAULT_TIMEOUT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDefaultTimeout() <em>Default Timeout</em>}' attribute
	 * @see #getDefaultTimeout()
	 * @generated
	 * @ordered
	 */
	protected Integer defaultTimeout = DEFAULT_TIMEOUT_EDEFAULT;

	/**
	 * The default value of the '{@link #getDecimalFormat() <em>Decimal Format</em>}' attribute
	 * @see #getDecimalFormat()
	 * @generated
	 * @ordered
	 */
	protected static final String DECIMAL_FORMAT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDecimalFormat() <em>Decimal Format</em>}' attribute
	 * @see #getDecimalFormat()
	 * @generated
	 * @ordered
	 */
	protected String decimalFormat = DECIMAL_FORMAT_EDEFAULT;

	/**
	 * The default value of the '{@link #getDateTimeFormat() <em>Date Time Format</em>}' attribute
	 * @see #getDateTimeFormat()
	 * @generated
	 * @ordered
	 */
	protected static final String DATE_TIME_FORMAT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDateTimeFormat() <em>Date Time Format</em>}' attribute
	 * @see #getDateTimeFormat()
	 * @generated
	 * @ordered
	 */
	protected String dateTimeFormat = DATE_TIME_FORMAT_EDEFAULT;

	/**
	 * The cached value of the '{@link #getIntegrationModule() <em>Integration Module</em>}' reference
	 * @see #getIntegrationModule()
	 * @generated
	 * @ordered
	 */
	protected IntegrationModule integrationModule;

	/**
	 * The default value of the '{@link #getDateFormat() <em>Date Format</em>}' attribute
	 * @see #getDateFormat()
	 * @generated
	 * @ordered
	 */
	protected static final String DATE_FORMAT_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDateFormat() <em>Date Format</em>}' attribute
	 * @see #getDateFormat()
	 * @generated
	 * @ordered
	 */
	protected String dateFormat = DATE_FORMAT_EDEFAULT;

	/**
	 * The default value of the '{@link #getDecimalSeparator() <em>Decimal Separator</em>}' attribute
	 * @see #getDecimalSeparator()
	 * @generated
	 * @ordered
	 */
	protected static final char DECIMAL_SEPARATOR_EDEFAULT = '\u0000';

	/**
	 * The cached value of the '{@link #getDecimalSeparator() <em>Decimal Separator</em>}' attribute
	 * @see #getDecimalSeparator()
	 * @generated
	 * @ordered
	 */
	protected char decimalSeparator = DECIMAL_SEPARATOR_EDEFAULT;

	/**
	 * The default value of the '{@link #getGroupingSeparator() <em>Grouping Separator</em>}' attribute
	 * @see #getGroupingSeparator()
	 * @generated
	 * @ordered
	 */
	protected static final char GROUPING_SEPARATOR_EDEFAULT = '\u0000';

	/**
	 * The cached value of the '{@link #getGroupingSeparator() <em>Grouping Separator</em>}' attribute
	 * @see #getGroupingSeparator()
	 * @generated
	 * @ordered
	 */
	protected char groupingSeparator = GROUPING_SEPARATOR_EDEFAULT;

	/**
	 * @generated
	 */
	protected IntegrationTestModuleImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.EObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.INTEGRATION_TEST_MODULE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDefaultTimeout()
	 * @generated
	 */
	@Override
	public Integer getDefaultTimeout() {
		return defaultTimeout;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#setDefaultTimeout(java.lang.Integer)
	 * @generated
	 */
	@Override
	public void setDefaultTimeout(Integer newDefaultTimeout) {
		final Integer oldDefaultTimeout = defaultTimeout;
		defaultTimeout = newDefaultTimeout;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_TEST_MODULE__DEFAULT_TIMEOUT,
					oldDefaultTimeout, defaultTimeout));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalFormat(java.lang.Integer)
	 * @generated
	 */
	@Override
	public String getDecimalFormat() {
		return decimalFormat;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#setDecimalFormat(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDecimalFormat(String newDecimalFormat) {
		final String oldDecimalFormat = decimalFormat;
		decimalFormat = newDecimalFormat;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_FORMAT,
					oldDecimalFormat, decimalFormat));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateTimeFormat()
	 * @generated
	 */
	@Override
	public String getDateTimeFormat() {
		return dateTimeFormat;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#setDateTimeFormat(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDateTimeFormat(String newDateTimeFormat) {
		final String oldDateTimeFormat = dateTimeFormat;
		dateTimeFormat = newDateTimeFormat;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_TEST_MODULE__DATE_TIME_FORMAT,
					oldDateTimeFormat, dateTimeFormat));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getIntegrationModule()
	 * @generated
	 */
	@Override
	public IntegrationModule getIntegrationModule() {
		if (integrationModule != null && integrationModule.eIsProxy()) {
			final InternalEObject oldIntegrationModule = (InternalEObject) integrationModule;
			integrationModule = (IntegrationModule) eResolveProxy(oldIntegrationModule);

			if ((integrationModule != oldIntegrationModule) && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, TestingPackage.INTEGRATION_TEST_MODULE__INTEGRATION_MODULE,
						oldIntegrationModule, integrationModule));
		}

		return integrationModule;
	}

	/**
	 * @return the integration module
	 * @generated
	 */
	public IntegrationModule basicGetIntegrationModule() {
		return integrationModule;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#
	 * setIntegrationModule(net.codecadenza.eclipse.model.project.IntegrationModule)
	 * @generated
	 */
	@Override
	public void setIntegrationModule(IntegrationModule newIntegrationModule) {
		final IntegrationModule oldIntegrationModule = integrationModule;
		integrationModule = newIntegrationModule;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_TEST_MODULE__INTEGRATION_MODULE,
					oldIntegrationModule, integrationModule));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDateFormat()
	 * @generated
	 */
	@Override
	public String getDateFormat() {
		return dateFormat;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#setDateFormat(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDateFormat(String newDateFormat) {
		final String oldDateFormat = dateFormat;
		dateFormat = newDateFormat;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_TEST_MODULE__DATE_FORMAT, oldDateFormat,
					dateFormat));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getDecimalSeparator()
	 * @generated
	 */
	@Override
	public char getDecimalSeparator() {
		return decimalSeparator;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#setDecimalSeparator(char)
	 * @generated
	 */
	@Override
	public void setDecimalSeparator(char newDecimalSeparator) {
		final char oldDecimalSeparator = decimalSeparator;
		decimalSeparator = newDecimalSeparator;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_SEPARATOR,
					oldDecimalSeparator, decimalSeparator));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#getGroupingSeparator()
	 * @generated
	 */
	@Override
	public char getGroupingSeparator() {
		return groupingSeparator;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.IntegrationTestModule#setGroupingSeparator(char)
	 * @generated
	 */
	@Override
	public void setGroupingSeparator(char newGroupingSeparator) {
		final char oldGroupingSeparator = groupingSeparator;
		groupingSeparator = newGroupingSeparator;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.INTEGRATION_TEST_MODULE__GROUPING_SEPARATOR,
					oldGroupingSeparator, groupingSeparator));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_TEST_MODULE__DEFAULT_TIMEOUT:
				return getDefaultTimeout();
			case TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_FORMAT:
				return getDecimalFormat();
			case TestingPackage.INTEGRATION_TEST_MODULE__DATE_TIME_FORMAT:
				return getDateTimeFormat();
			case TestingPackage.INTEGRATION_TEST_MODULE__INTEGRATION_MODULE:
				if (resolve)
					return getIntegrationModule();

				return basicGetIntegrationModule();
			case TestingPackage.INTEGRATION_TEST_MODULE__DATE_FORMAT:
				return getDateFormat();
			case TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_SEPARATOR:
				return getDecimalSeparator();
			case TestingPackage.INTEGRATION_TEST_MODULE__GROUPING_SEPARATOR:
				return getGroupingSeparator();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_TEST_MODULE__DEFAULT_TIMEOUT:
				setDefaultTimeout((Integer) newValue);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_FORMAT:
				setDecimalFormat((String) newValue);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__DATE_TIME_FORMAT:
				setDateTimeFormat((String) newValue);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__INTEGRATION_MODULE:
				setIntegrationModule((IntegrationModule) newValue);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__DATE_FORMAT:
				setDateFormat((String) newValue);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_SEPARATOR:
				setDecimalSeparator((Character) newValue);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__GROUPING_SEPARATOR:
				setGroupingSeparator((Character) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_TEST_MODULE__DEFAULT_TIMEOUT:
				setDefaultTimeout(DEFAULT_TIMEOUT_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_FORMAT:
				setDecimalFormat(DECIMAL_FORMAT_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__DATE_TIME_FORMAT:
				setDateTimeFormat(DATE_TIME_FORMAT_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__INTEGRATION_MODULE:
				setIntegrationModule((IntegrationModule) null);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__DATE_FORMAT:
				setDateFormat(DATE_FORMAT_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_SEPARATOR:
				setDecimalSeparator(DECIMAL_SEPARATOR_EDEFAULT);
				return;
			case TestingPackage.INTEGRATION_TEST_MODULE__GROUPING_SEPARATOR:
				setGroupingSeparator(GROUPING_SEPARATOR_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case TestingPackage.INTEGRATION_TEST_MODULE__DEFAULT_TIMEOUT:
				return defaultTimeout != null;
			case TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_FORMAT:
				return decimalFormat != null;
			case TestingPackage.INTEGRATION_TEST_MODULE__DATE_TIME_FORMAT:
				return dateTimeFormat != null;
			case TestingPackage.INTEGRATION_TEST_MODULE__INTEGRATION_MODULE:
				return integrationModule != null;
			case TestingPackage.INTEGRATION_TEST_MODULE__DATE_FORMAT:
				return dateFormat != null;
			case TestingPackage.INTEGRATION_TEST_MODULE__DECIMAL_SEPARATOR:
				return decimalSeparator != DECIMAL_SEPARATOR_EDEFAULT;
			case TestingPackage.INTEGRATION_TEST_MODULE__GROUPING_SEPARATOR:
				return groupingSeparator != GROUPING_SEPARATOR_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final StringBuilder result = new StringBuilder(super.toString());
		result.append(" (defaultTimeout: ");
		result.append(defaultTimeout);
		result.append(", decimalFormat: ");
		result.append(decimalFormat);
		result.append(", dateTimeFormat: ");
		result.append(dateTimeFormat);
		result.append(", dateFormat: ");
		result.append(dateFormat);
		result.append(", decimalSeparator: ");
		result.append(decimalSeparator);
		result.append(", groupingSeparator: ");
		result.append(groupingSeparator);
		result.append(')');

		return result.toString();
	}

}
