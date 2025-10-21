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

import java.util.List;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.SeleniumDriver;
import net.codecadenza.eclipse.model.testing.SeleniumTestModule;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Selenium Test Module</b></em>'.
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.SeleniumTestModuleImpl#getDriver <em>Driver</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.SeleniumTestModuleImpl#getDriverPath <em>Driver Path</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.SeleniumTestModuleImpl#isMaximizeWindow <em>Maximize Window</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.SeleniumTestModuleImpl#getImplicitWaitTime <em>Implicit Wait
 * Time</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.impl.SeleniumTestModuleImpl#getPageLoadTime <em>Page Load Time</em>}</li>
 * </ul>
 * @generated
 */
public class SeleniumTestModuleImpl extends AbstractTestModuleImpl implements SeleniumTestModule {
	/**
	 * The default value of the '{@link #getDriver() <em>Driver</em>}' attribute
	 * @see #getDriver()
	 * @generated
	 * @ordered
	 */
	protected static final SeleniumDriver DRIVER_EDEFAULT = SeleniumDriver.CHROME;

	/**
	 * The cached value of the '{@link #getDriver() <em>Driver</em>}' attribute
	 * @see #getDriver()
	 * @generated
	 * @ordered
	 */
	protected SeleniumDriver driver = DRIVER_EDEFAULT;

	/**
	 * The default value of the '{@link #getDriverPath() <em>Driver Path</em>}' attribute
	 * @see #getDriverPath()
	 * @generated
	 * @ordered
	 */
	protected static final String DRIVER_PATH_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDriverPath() <em>Driver Path</em>}' attribute
	 * @see #getDriverPath()
	 * @generated
	 * @ordered
	 */
	protected String driverPath = DRIVER_PATH_EDEFAULT;

	/**
	 * The default value of the '{@link #isMaximizeWindow() <em>Maximize Window</em>}' attribute
	 * @see #isMaximizeWindow()
	 * @generated
	 * @ordered
	 */
	protected static final boolean MAXIMIZE_WINDOW_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isMaximizeWindow() <em>Maximize Window</em>}' attribute
	 * @see #isMaximizeWindow()
	 * @generated
	 * @ordered
	 */
	protected boolean maximizeWindow = MAXIMIZE_WINDOW_EDEFAULT;

	/**
	 * The default value of the '{@link #getImplicitWaitTime() <em>Implicit Wait Time</em>}' attribute
	 * @see #getImplicitWaitTime()
	 * @generated
	 * @ordered
	 */
	protected static final int IMPLICIT_WAIT_TIME_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getImplicitWaitTime() <em>Implicit Wait Time</em>}' attribute
	 * @see #getImplicitWaitTime()
	 * @generated
	 * @ordered
	 */
	protected int implicitWaitTime = IMPLICIT_WAIT_TIME_EDEFAULT;

	/**
	 * The default value of the '{@link #getPageLoadTime() <em>Page Load Time</em>}' attribute
	 * @see #getPageLoadTime()
	 * @generated
	 * @ordered
	 */
	protected static final int PAGE_LOAD_TIME_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getPageLoadTime() <em>Page Load Time</em>}' attribute
	 * @see #getPageLoadTime()
	 * @generated
	 * @ordered
	 */
	protected int pageLoadTime = PAGE_LOAD_TIME_EDEFAULT;

	/**
	 * @generated
	 */
	protected SeleniumTestModuleImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.SELENIUM_TEST_MODULE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriver()
	 * @generated
	 */
	@Override
	public SeleniumDriver getDriver() {
		return driver;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#setDriver(net.codecadenza.eclipse.model.testing.SeleniumDriver)
	 * @generated
	 */
	@Override
	public void setDriver(SeleniumDriver newDriver) {
		final SeleniumDriver oldDriver = driver;
		driver = newDriver == null ? DRIVER_EDEFAULT : newDriver;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.SELENIUM_TEST_MODULE__DRIVER, oldDriver, driver));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriverPath()
	 * @generated
	 */
	@Override
	public String getDriverPath() {
		return driverPath;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#setDriverPath(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDriverPath(String newDriverPath) {
		final String oldDriverPath = driverPath;
		driverPath = newDriverPath;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.SELENIUM_TEST_MODULE__DRIVER_PATH, oldDriverPath,
					driverPath));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#isMaximizeWindow()
	 * @generated
	 */
	@Override
	public boolean isMaximizeWindow() {
		return maximizeWindow;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#setMaximizeWindow(boolean)
	 * @generated
	 */
	@Override
	public void setMaximizeWindow(boolean newMaximizeWindow) {
		final boolean oldMaximizeWindow = maximizeWindow;
		maximizeWindow = newMaximizeWindow;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.SELENIUM_TEST_MODULE__MAXIMIZE_WINDOW,
					oldMaximizeWindow, maximizeWindow));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#getImplicitWaitTime()
	 * @generated
	 */
	@Override
	public int getImplicitWaitTime() {
		return implicitWaitTime;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#setImplicitWaitTime(int)
	 * @generated
	 */
	@Override
	public void setImplicitWaitTime(int newImplicitWaitTime) {
		final int oldImplicitWaitTime = implicitWaitTime;
		implicitWaitTime = newImplicitWaitTime;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.SELENIUM_TEST_MODULE__IMPLICIT_WAIT_TIME,
					oldImplicitWaitTime, implicitWaitTime));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#getPageLoadTime()
	 * @generated
	 */
	@Override
	public int getPageLoadTime() {
		return pageLoadTime;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#setPageLoadTime(int)
	 * @generated
	 */
	@Override
	public void setPageLoadTime(int newPageLoadTime) {
		final int oldPageLoadTime = pageLoadTime;
		pageLoadTime = newPageLoadTime;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, TestingPackage.SELENIUM_TEST_MODULE__PAGE_LOAD_TIME, oldPageLoadTime,
					pageLoadTime));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case TestingPackage.SELENIUM_TEST_MODULE__DRIVER:
				return getDriver();
			case TestingPackage.SELENIUM_TEST_MODULE__DRIVER_PATH:
				return getDriverPath();
			case TestingPackage.SELENIUM_TEST_MODULE__MAXIMIZE_WINDOW:
				return isMaximizeWindow();
			case TestingPackage.SELENIUM_TEST_MODULE__IMPLICIT_WAIT_TIME:
				return getImplicitWaitTime();
			case TestingPackage.SELENIUM_TEST_MODULE__PAGE_LOAD_TIME:
				return getPageLoadTime();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case TestingPackage.SELENIUM_TEST_MODULE__DRIVER:
				setDriver((SeleniumDriver) newValue);
				return;
			case TestingPackage.SELENIUM_TEST_MODULE__DRIVER_PATH:
				setDriverPath((String) newValue);
				return;
			case TestingPackage.SELENIUM_TEST_MODULE__MAXIMIZE_WINDOW:
				setMaximizeWindow((Boolean) newValue);
				return;
			case TestingPackage.SELENIUM_TEST_MODULE__IMPLICIT_WAIT_TIME:
				setImplicitWaitTime((Integer) newValue);
				return;
			case TestingPackage.SELENIUM_TEST_MODULE__PAGE_LOAD_TIME:
				setPageLoadTime((Integer) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case TestingPackage.SELENIUM_TEST_MODULE__DRIVER:
				setDriver(DRIVER_EDEFAULT);
				return;
			case TestingPackage.SELENIUM_TEST_MODULE__DRIVER_PATH:
				setDriverPath(DRIVER_PATH_EDEFAULT);
				return;
			case TestingPackage.SELENIUM_TEST_MODULE__MAXIMIZE_WINDOW:
				setMaximizeWindow(MAXIMIZE_WINDOW_EDEFAULT);
				return;
			case TestingPackage.SELENIUM_TEST_MODULE__IMPLICIT_WAIT_TIME:
				setImplicitWaitTime(IMPLICIT_WAIT_TIME_EDEFAULT);
				return;
			case TestingPackage.SELENIUM_TEST_MODULE__PAGE_LOAD_TIME:
				setPageLoadTime(PAGE_LOAD_TIME_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case TestingPackage.SELENIUM_TEST_MODULE__DRIVER:
				return driver != DRIVER_EDEFAULT;
			case TestingPackage.SELENIUM_TEST_MODULE__DRIVER_PATH:
				return driverPath != null;
			case TestingPackage.SELENIUM_TEST_MODULE__MAXIMIZE_WINDOW:
				return maximizeWindow != MAXIMIZE_WINDOW_EDEFAULT;
			case TestingPackage.SELENIUM_TEST_MODULE__IMPLICIT_WAIT_TIME:
				return implicitWaitTime != IMPLICIT_WAIT_TIME_EDEFAULT;
			case TestingPackage.SELENIUM_TEST_MODULE__PAGE_LOAD_TIME:
				return pageLoadTime != PAGE_LOAD_TIME_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.impl.AbstractTestModuleImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (driver: ");
		result.append(driver);
		result.append(", driverPath: ");
		result.append(driverPath);
		result.append(", maximizeWindow: ");
		result.append(maximizeWindow);
		result.append(", implicitWaitTime: ");
		result.append(implicitWaitTime);
		result.append(", pageLoadTime: ");
		result.append(pageLoadTime);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.SeleniumTestModule#getGUITestCases()
	 * @generated not
	 */
	@Override
	public List<GUITestCase> getGUITestCases() {
		return getProject().getAllGUITestCases();
	}

}
