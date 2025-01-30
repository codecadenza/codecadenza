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
package net.codecadenza.eclipse.model.testing;

import java.util.List;

/**
 * A representation of the model object '<em><b>Selenium Test Module</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriver <em>Driver</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriverPath <em>Driver Path</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#isMaximizeWindow <em>Maximize Window</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getImplicitWaitTime <em>Implicit Wait Time</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getPageLoadTime <em>Page Load Time</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule()
 * @model
 * @generated
 */
public interface SeleniumTestModule extends AbstractTestModule {
	/**
	 * Return the value of the '<em><b>Driver</b></em>' attribute
	 * @return the value of the '<em>Driver</em>' attribute
	 * @see #setDriver(SeleniumDriver)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_Driver()
	 * @model
	 * @generated
	 */
	SeleniumDriver getDriver();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriver <em>Driver</em>}' attribute
	 * @param value the new value of the '<em>Driver</em>' attribute
	 * @see #getDriver()
	 * @generated
	 */
	void setDriver(SeleniumDriver value);

	/**
	 * Return the value of the '<em><b>Driver Path</b></em>' attribute
	 * @return the value of the '<em>Driver Path</em>' attribute
	 * @see #setDriverPath(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_DriverPath()
	 * @model
	 * @generated
	 */
	String getDriverPath();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getDriverPath <em>Driver Path</em>}'
	 * attribute
	 * @param value the new value of the '<em>Driver Path</em>' attribute
	 * @see #getDriverPath()
	 * @generated
	 */
	void setDriverPath(String value);

	/**
	 * Return the value of the '<em><b>Maximize Window</b></em>' attribute
	 * @return the value of the '<em>Maximize Window</em>' attribute
	 * @see #setMaximizeWindow(boolean)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_MaximizeWindow()
	 * @model
	 * @generated
	 */
	boolean isMaximizeWindow();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#isMaximizeWindow <em>Maximize
	 * Window</em>}' attribute
	 * @param value the new value of the '<em>Maximize Window</em>' attribute
	 * @see #isMaximizeWindow()
	 * @generated
	 */
	void setMaximizeWindow(boolean value);

	/**
	 * Return the value of the '<em><b>Implicit Wait Time</b></em>' attribute
	 * @return the value of the '<em>Implicit Wait Time</em>' attribute
	 * @see #setImplicitWaitTime(int)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_ImplicitWaitTime()
	 * @model
	 * @generated
	 */
	int getImplicitWaitTime();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getImplicitWaitTime <em>Implicit Wait
	 * Time</em>}' attribute
	 * @param value the new value of the '<em>Implicit Wait Time</em>' attribute
	 * @see #getImplicitWaitTime()
	 * @generated
	 */
	void setImplicitWaitTime(int value);

	/**
	 * Return the value of the '<em><b>Page Load Time</b></em>' attribute
	 * @return the value of the '<em>Page Load Time</em>' attribute
	 * @see #setPageLoadTime(int)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getSeleniumTestModule_PageLoadTime()
	 * @model
	 * @generated
	 */
	int getPageLoadTime();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.SeleniumTestModule#getPageLoadTime <em>Page Load
	 * Time</em>}' attribute
	 * @param value the new value of the '<em>Page Load Time</em>' attribute
	 * @see #getPageLoadTime()
	 * @generated
	 */
	void setPageLoadTime(int value);

	/**
	 * @return a list containing all GUI test cases of this test module
	 * @generated not
	 */
	List<GUITestCase> getGUITestCases();

}
